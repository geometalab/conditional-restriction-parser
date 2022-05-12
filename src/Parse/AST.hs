module Parse.AST where
import Data.Hourglass (WeekDay, TimeOfDay)

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

data RuleType = Normal
              | Additional
 deriving (Eq, Show)

data RuleSequence = RuleSequence RuleType SelectorSequence OHState
 deriving (Eq, Show)

type WeekdaySelector = [WeekdayRange]
type TimeSelector = [TimeSpan]

data SelectorSequence = TwentyFourSeven
                      | WeekdaySel WeekdaySelector
                      | TimeSel TimeSelector
                      | WeekdayTime WeekdaySelector TimeSelector
 deriving (Eq, Show)

data WeekdayRange = SingleDay WeekDay
                  | WdayRange WeekDay WeekDay
 deriving (Eq, Show)

data TimeSpan = Moment TimeOfDay
              | Span TimeOfDay TimeOfDay
 deriving (Eq, Show)
