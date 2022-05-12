{-# LANGUAGE LambdaCase #-}
module Evaluate.Evaluator where

import Evaluate.InputData (Type (..), Value (..), ID)
import Parse.AST
import Parse.Lib (Result(..))
import Util.Monad (findM, allM)
import Data.Hourglass
import Data.List (nub)

type EvalError = Either String [(ID, Type)]

result :: [(ID, Value)] -> ConditionalRestriction -> Result EvalError (Maybe Token) -- TODO combine needed data output
result ds (ConditionalRestriction exprs) = findM (\(Expression _ conds) -> allM (fulfills ds) conds) exprs >>= \case
  Nothing -> Ok Nothing
  Just (Expression tok _) -> Ok $ Just tok

fulfills :: [(ID, Value)] -> Condition -> Result EvalError Bool
fulfills ds (OH _) = undefined
fulfills ds (Comparison tok op val) = case lookup tok ds of
  Just (VNum d) -> Ok $ case op of
    Gt -> d > val
    Lt -> d < val
    GtEq -> d >= val
    LtEq -> d <= val
    Eq -> d == val
  Just _ -> Err . Left $ "Incorrect input type for " ++ tok
  Nothing -> Err $ Right [(tok, TNum)]
fulfills ds (Absolute tok) = case lookup tok ds of
  Just (VBool b) -> Ok b
  Just _ -> Err . Left $ "Incorrect input type for " ++ tok
  Nothing -> Err $ Right [(tok, TBool)]


timeIn :: Timeable t => t -> OpeningHours -> Maybe Bool -- TODO support extended time
timeIn t oh = Just $ timeInSelector time (ohTimes oh !! fromEnum weekday)
  where
    date = timeGetDate t
    time = timeGetTimeOfDay t
    weekday = getWeekDay date

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
    match_t (Span t1 t2) = t >= t1 && t <= t2

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
