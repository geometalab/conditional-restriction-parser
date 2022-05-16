{-# LANGUAGE LambdaCase #-}

-- | Functions to evaluate conditional restrictions.
module ConditionalRestriction.Internal.Evaluate where

import ConditionalRestriction.Parse.AST
  ( ComparisonOp (Eq, Gt, GtEq, Lt, LtEq),
    Condition (..),
    ConditionalRestriction (..),
    Expression (Expression),
    OpeningHours (..),
    RuleSequence (RuleSequence),
    RuleType (Additional, Normal),
    SelectorSequence
      ( TimeSel,
        TwentyFourSeven,
        WeekdaySel,
        WeekdayTime
      ),
    TimeSelector,
    TimeSpan (..),
    Token,
    WeekdayRange (SingleDay, WdayRange),
    WeekdaySelector,
  )
import ConditionalRestriction.Parse.InputData
  ( ID,
    Type (..),
    Value (..),
  )
import ConditionalRestriction.Result (Result (..))
import Data.Hourglass
  ( DateTime,
    TimeOfDay (TimeOfDay),
    WeekDay (Saturday, Sunday),
    getWeekDay,
    timeGetDate,
    timeGetTimeOfDay,
  )
import Data.List (nub)

-- | Plain text error message.
type ErrorMsg = String

-- | The 'result' function takes input data in the form of ('ID', 'Value') and a 'ConditionalRestriction' and returns
-- the result of that 'ConditionalRestriction' when applied to the input data given. If data needed for the evaluation
-- is missing or of the wrong type, it will return a list of error messages and a list of missing data types instead.
--
-- Note that this function will accept incomplete data if it is enough to evaluate the expression, but will always return
-- a complete list of needed data types.
result :: [(ID, Value)] -> ConditionalRestriction -> Result ([ErrorMsg], [(ID, Type)]) (Maybe Token)
result ds (ConditionalRestriction exprs) =
  find_r (\(Expression _ conds) -> all_r (fulfills ds) conds) (reverse exprs) >>= \case
    Nothing -> Ok Nothing
    Just (Expression tok _) -> Ok $ Just tok
  where
    find_r f (x : xs) = case f x of
      Ok True -> Ok $ Just x
      Ok False -> find_r f xs
      Err (msgs, needed) -> case find_r f xs of
        Err (msgs', needed') -> Err (msgs ++ msgs', nub $ needed ++ needed')
        Ok v -> Err (msgs, needed)
    find_r f [] = Ok Nothing

    all_r f (x : xs) = case f x of
      Ok True -> all_r f xs
      Ok False -> False <$ all_r f xs
      Err (Left msg) -> case all_r f xs of
        Ok _ -> Err ([msg], [])
        Err (msgs, neededs) -> Err (msg : msgs, neededs)
      Err (Right needed) -> case all_r f xs of
        Ok _ -> Err ([], [needed])
        Err (msgs, neededs) -> Err (msgs, needed : neededs)
    all_r f [] = Ok True

-- | The 'fulfills' function takes input data in the form of ('ID', 'Value') and a 'Condition' and returns
-- whether that condition is fulfilled. If some data is missing, it will return the missing data 'ID' and 'Type'
-- and if the given data is of the wrong type, it will return an error message.
fulfills :: [(ID, Value)] -> Condition -> Result (Either ErrorMsg (ID, Type)) Bool
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

-- | The 'timeIn' function returns wheter a 'DateTime' is within given 'OpeningHours'. Unknown values count as not
-- within the opening hours.
timeIn :: DateTime -> OpeningHours -> Bool
timeIn t oh =
  timeInSelector time (ohTimes oh !! fromEnum weekday)
    || timeExtendedInSelector time (ohTimes oh !! fromEnum previous_weekday)
  where
    date = timeGetDate t
    time = timeGetTimeOfDay t
    weekday = getWeekDay date
    previous_weekday = if weekday == minBound then maxBound else pred weekday

ohTimes :: OpeningHours -> [TimeSelector]
ohTimes (OpeningHours rs) = foldr set_time_ranges [[] | _ <- range Sunday Saturday] (reverse rs)
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
addTimespan ts sel = add_timespan (explicitExtended ts) (map explicitExtended sel)
  where
    add_timespan (Moment t) sel@(Moment t' : ts) | t == t' = sel
    add_timespan (Moment t) (Moment t' : ts) = Moment t' : add_timespan (Moment t) ts
    add_timespan (Moment t) sel@(Span t1 t2 : ts) | t >= t1 && t <= t2 = sel
    add_timespan (Moment t) (Span t1 t2 : ts) = Span t1 t2 : add_timespan (Moment t) ts
    add_timespan (Span t1 t2) (Moment t : ts) | t >= t1 && t <= t2 = Span t1 t2 : ts
    add_timespan (Span t1 t2) (Moment t : ts) = Moment t : add_timespan (Span t1 t2) ts
    add_timespan (Span ta1 ta2) (Span tb1 tb2 : ts) | ta1 <= tb1 && ta2 >= tb2 = Span ta1 ta2 : ts
    add_timespan (Span ta1 ta2) (Span tb1 tb2 : ts) | ta1 >= tb1 && ta2 <= tb2 = Span tb1 tb2 : ts
    add_timespan (Span ta1 ta2) (Span tb1 tb2 : ts) | ta1 <= tb1 && ta2 <= tb2 = Span ta1 tb2 : ts
    add_timespan (Span ta1 ta2) (Span tb1 tb2 : ts) | ta1 >= tb1 && ta2 >= tb2 = Span tb1 ta2 : ts
    add_timespan (Span ta1 ta2) (Span tb1 tb2 : ts) = Span tb1 tb2 : add_timespan (Span ta1 ta2) ts
    add_timespan x [] = [x]

subtractTimespan :: TimeSpan -> TimeSelector -> TimeSelector
subtractTimespan ts sel = subtract_timespan (explicitExtended ts) (map explicitExtended sel)
  where
    subtract_timespan (Moment t) sel@(Moment t' : ts) | t == t' = ts
    subtract_timespan (Moment t) (Moment t' : ts) = Moment t' : subtract_timespan (Moment t) ts
    subtract_timespan (Moment t) sel@(Span t1 t2 : ts) | t >= t1 && t <= t2 = sel -- don't subtract single moments from time spans
    subtract_timespan (Moment t) (Span t1 t2 : ts) = Span t1 t2 : subtract_timespan (Moment t) ts
    subtract_timespan (Span t1 t2) (Moment t : ts) | t >= t1 && t <= t2 = subtract_timespan (Span t1 t2) ts
    subtract_timespan (Span t1 t2) (Moment t : ts) = Moment t : subtract_timespan (Span t1 t2) ts
    subtract_timespan (Span ta1 ta2) (Span tb1 tb2 : ts) | ta1 <= tb1 && ta2 >= tb2 = Span ta1 tb1 : Span tb2 ta2 : ts
    subtract_timespan (Span ta1 ta2) (Span tb1 tb2 : ts) | ta1 >= tb1 && ta2 <= tb2 = Span tb1 ta1 : Span ta2 tb2 : ts
    subtract_timespan (Span ta1 ta2) (Span tb1 tb2 : ts) | ta1 <= tb1 && ta2 <= tb2 = Span ta2 tb2 : subtract_timespan (Span ta1 ta2) ts
    subtract_timespan (Span ta1 ta2) (Span tb1 tb2 : ts) | ta1 >= tb1 && ta2 >= tb2 = Span tb1 ta1 : subtract_timespan (Span ta1 ta2) ts
    subtract_timespan (Span ta1 ta2) (Span tb1 tb2 : ts) = Span tb1 tb2 : subtract_timespan (Span ta1 ta2) ts
    subtract_timespan x [] = []

explicitExtended :: TimeSpan -> TimeSpan
explicitExtended (Span t1 t2@(TimeOfDay h m s n)) | t1 >= t2 = Span t1 (TimeOfDay (h + 24) m s n)
explicitExtended other = other

mapDays :: [WeekDay] -> (a -> a) -> [a] -> [a]
mapDays days f = zipWith (\d x -> (if d `elem` days then f x else x)) (range Sunday Saturday)

combineWeekdays :: WeekdaySelector -> [WeekDay]
combineWeekdays = nub . combine
  where
    combine (SingleDay day : wdrs) = day : combineWeekdays wdrs
    combine (WdayRange from to : wdrs) = range from to ++ combineWeekdays wdrs
    combine [] = []

range :: (Ord a, Enum a, Bounded a) => a -> a -> [a]
range a b | b < a = [a ..] ++ [minBound .. b]
range a b = [a .. b]
