{-|
AST for conditional restrictions and incomplete AST for opening hours.
-}
module ConditionalRestriction.Parse.AST where

import Data.Hourglass (WeekDay, TimeOfDay)

-- | A single token.
type Token = String

-- | AST representation of a conditional restriction.
newtype ConditionalRestriction = ConditionalRestriction [Expression]
 deriving (Eq, Show)

-- | AST representation of a conditional restriction expression, containing a value and conditions for that value
data Expression = Expression
  -- | The value
  Token
  -- | The conditions. All conditions must be met when evaluating.
  [Condition]
 deriving (Eq, Show)

-- | AST representation of a condition.
data Condition
  -- | An 'OpeningHours' condition. When evaluating, the given time must be within those opening hours.
  = OH OpeningHours
  -- | A comparison. Looks something like @"weight > 3.0"@
  | Comparison Token ComparisonOp Double
  -- | An absolute condition, e.g. @"wet"@, @"disabled"@.
  | Absolute Token
 deriving (Eq, Show)

-- | A comparison operator.
data ComparisonOp = Gt | Lt | GtEq | LtEq | Eq
 deriving (Eq, Show)

-- | AST representation of opening hours. Not complete.
newtype OpeningHours = OpeningHours [RuleSequence]
 deriving (Eq, Show)

-- | Opening hour state. True\/False if known to be open/closed, Nothing if unknown.
type OHState = Maybe Bool

-- | Type of opening hour rule.
data RuleType
  -- | First rule or rules separated by ";".
  = Normal
  -- | Rules separated by ",".
  | Additional
 deriving (Eq, Show)

-- | AST representation of a rule sequence.
data RuleSequence = RuleSequence RuleType SelectorSequence OHState
 deriving (Eq, Show)

-- | AST representation of a weekday selector (e.g. @"Sa-Di, Th"@).
type WeekdaySelector = [WeekdayRange]

-- | AST representation of a time selector (e.g. @"18:00-20:00, 21:00-03:00"@).
type TimeSelector = [TimeSpan]

-- | AST representation of a selector sequence (e.g. @"24/7"@, @"We-Su 18:00-20:00"@).
data SelectorSequence = TwentyFourSeven
                      | WeekdaySel WeekdaySelector
                      | TimeSel TimeSelector
                      | WeekdayTime WeekdaySelector TimeSelector
 deriving (Eq, Show)

-- | AST representation of a weekday range.
data WeekdayRange = SingleDay WeekDay
                  | WdayRange WeekDay WeekDay
 deriving (Eq, Show)

-- | AST representation of time span.
data TimeSpan = Moment TimeOfDay
              | Span TimeOfDay TimeOfDay
 deriving (Eq, Show)
