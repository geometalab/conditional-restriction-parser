module Parse.AST where

type Token = String

newtype ConditionalRestriction = ConditionalRestriction [Expression]
 deriving (Eq, Show)

data Expression = Expression Token [Condition]
 deriving (Eq, Show)

data Condition
  = OH OpeningHours
  | Comparison Token ComparisonOp Double
  | Absolute Token
 deriving (Eq, Show)

data ComparisonOp = Gt | Lt | GtEq | LtEq | Eq
 deriving (Eq, Show)

newtype OpeningHours = OpeningHours [RuleSequence]
 deriving (Eq, Show)

type OHState = Maybe Bool

data RuleSequence = RuleSequence SelectorSequence OHState
 deriving (Eq, Show)

type WeekdaySelector = [WeekdayRange]
type TimeSelector = [TimeSpan]

data SelectorSequence = TwentyFourSeven
                      | WeekdaySel WeekdaySelector
                      | TimeSel TimeSelector
                      | WeekdayTime WeekdaySelector TimeSelector
 deriving (Eq, Show)

data WeekdayRange = SingleDay Wday
                  | WdayRange Wday Wday
 deriving (Eq, Show)

data Wday = Su | Mo | Tu | We | Th | Fr | Sa
 deriving (Eq, Show)

data TimeSpan = Moment Time
              | Span Time Time
 deriving (Eq, Show)

data Time = Time {hour :: Int, minute :: Int}
 deriving (Eq, Show)
