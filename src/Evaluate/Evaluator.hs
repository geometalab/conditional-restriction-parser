{-# LANGUAGE LambdaCase #-}
module Evaluate.Evaluator where

import Parse.InputData (Type (..), Value (..), ID)
import Parse.AST
import Parse.Lib (Result(..))
import Util.Monad (findM, allM)
import Data.Hourglass
import Data.List (nub)

type EvalError = Either String [(ID, Type)]

result :: [(ID, Value)] -> ConditionalRestriction -> Result ([String], [(ID, Type)]) (Maybe Token) -- TODO combine needed data output
result ds (ConditionalRestriction exprs) = find_r (\(Expression _ conds) -> all_r (fulfills ds) conds) exprs >>= \case
  Nothing -> Ok Nothing
  Just (Expression tok _) -> Ok $ Just tok
 where
   find_r f (x:xs) = case f x of
     Ok True -> Ok $ Just x
     Ok False -> find_r f xs
     Err (msgs, needed) -> case find_r f xs of
       Err (msgs', needed') -> Err (msgs ++ msgs', nub $ needed ++ needed')
       Ok v -> Err (msgs, needed)
   find_r f [] = Ok Nothing

   all_r f (x:xs) = case f x of
     Ok True -> all_r f xs
     Ok False -> False <$ all_r f xs
     Err (Left msg) -> case all_r f xs of
       Ok _ -> Err ([msg], [])
       Err (msgs, neededs) -> Err (msg:msgs, neededs)
     Err (Right needed) -> case all_r f xs of
       Ok _ -> Err ([], [needed])
       Err (msgs, neededs) -> Err (msgs, needed:neededs)
   all_r f [] = Ok True

fulfills :: [(ID, Value)] -> Condition -> Result (Either String (ID, Type)) Bool
fulfills ds (OH oh) = case lookup "time" ds of
  Just (VTime t) -> Ok $ timeIn t oh
  Just _ -> Err . Left $ "Incorrect input type for time"
  Nothing -> Err $ Right ("time", TTime)
fulfills ds (Comparison tok op val) = case lookup tok ds of
  Just (VNum d) -> Ok $ case op of
    Gt -> d > val
    Lt -> d < val
    GtEq -> d >= val
    LtEq -> d <= val
    Eq -> d == val
  Just _ -> Err . Left $ "Incorrect input type for " ++ tok
  Nothing -> Err $ Right (tok, TNum)
fulfills ds (Absolute tok) = case lookup tok ds of
  Just (VBool b) -> Ok b
  Just _ -> Err . Left $ "Incorrect input type for " ++ tok
  Nothing -> Err $ Right (tok, TBool)


timeIn :: Timeable t => t -> OpeningHours -> Bool
timeIn t oh = timeInSelector time (ohTimes oh !! fromEnum weekday)
           || timeExtendedInSelector time (ohTimes oh !! fromEnum previous_weekday)
  where
    date = timeGetDate t
    time = timeGetTimeOfDay t
    weekday = getWeekDay date
    previous_weekday = if weekday == minBound then maxBound else pred weekday

ohTimes :: OpeningHours -> [TimeSelector]
ohTimes (OpeningHours rs) = foldr set_time_ranges [[] | _ <- range Sunday Saturday] rs
 where
  set_time_ranges (RuleSequence t TwentyFourSeven (Just o)) = map (integrate t o [Span min_time max_time])
  set_time_ranges (RuleSequence t (WeekdaySel wdrs) (Just o)) = mapDays (combineWeekdays wdrs) (integrate t o [Span min_time max_time])
  set_time_ranges (RuleSequence t (TimeSel ts) (Just o)) = map (integrate t o ts)
  set_time_ranges (RuleSequence t (WeekdayTime wdrs ts) (Just o)) = mapDays (combineWeekdays wdrs) (integrate t o ts)
  set_time_ranges _ = id
  min_time = TimeOfDay 00 00 00 0
  max_time = TimeOfDay 23 59 59 999999999
  integrate Normal = override
  integrate Additional = combine
  override True = const
  override False = \_ -> const []
  combine True = foldr addTimespan
  combine False = foldr subtractTimespan


timeInSelector :: TimeOfDay -> TimeSelector -> Bool
timeInSelector t = any match_t
  where
    match_t (Moment t') = t' == t
    match_t (Span t1 t2) = t >= t1 && (t <= t2 || t1 > t2)

timeExtendedInSelector :: TimeOfDay -> TimeSelector -> Bool
timeExtendedInSelector t = any match_t
  where
    match_t (Moment (TimeOfDay h' m' s' n')) = t == TimeOfDay (h' - 24) m' s' n'
    match_t (Span t1 t2) | t1 > t2 = t <= t2
    match_t (Span t1 (TimeOfDay h' m' s' n')) | h' >= 24 = t < TimeOfDay (h' - 24) m' s' n'
    match_t _ = False

addTimespan :: TimeSpan -> TimeSelector -> TimeSelector
addTimespan (Moment t) sel@(Moment t':ts) | t == t' = sel
addTimespan (Moment t) (Moment t':ts) = Moment t' : addTimespan (Moment t) ts
addTimespan (Moment t) sel@(Span t1 t2:ts) | t >= t1 && t <= t2 = sel
addTimespan (Moment t) (Span t1 t2:ts) = Span t1 t2 : addTimespan (Moment t) ts
addTimespan (Span t1 t2) (Moment t:ts) | t >= t1 && t <= t2 = Span t1 t2 : ts
addTimespan (Span t1 t2) (Moment t:ts) = Moment t : addTimespan (Span t1 t2) ts
addTimespan (Span ta1 ta2) (Span tb1 tb2:ts) = undefined
addTimespan x [] = [x]

subtractTimespan :: TimeSpan -> TimeSelector -> TimeSelector
subtractTimespan = undefined

mapDays :: [WeekDay] -> (a -> a) -> [a] -> [a]
mapDays days f = zipWith (\d x -> (if d `elem` days then f x else x)) (range Sunday Saturday)

combineWeekdays :: WeekdaySelector -> [WeekDay]
combineWeekdays = nub . combine
  where
    combine (SingleDay day:wdrs) = day : combineWeekdays wdrs
    combine (WdayRange from to:wdrs) = range from to ++ combineWeekdays wdrs
    combine [] = []

range :: (Ord a, Enum a, Bounded a) => a -> a -> [a]
range a b | b < a = [a..] ++ [minBound..b]
range a b = [a..b]
