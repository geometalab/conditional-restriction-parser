module Arbitrary.ConditionalRestriction.Parse.AST where

import ConditionalRestriction.Parse.AST
  ( ComparisonOp (..),
    Condition (..),
    ConditionalRestriction (..),
    Expression (..),
    OpeningHours (OpeningHours),
    RuleSequence (RuleSequence),
    RuleType (..),
    SelectorSequence (..),
    TimeSpan (..),
    WeekdayRange (..),
  )
import Control.Applicative (Alternative (some))
import Data.Hourglass (TimeOfDay (TimeOfDay), WeekDay (Saturday, Sunday))
import Data.Maybe (maybeToList)
import Test.QuickCheck
  ( Arbitrary (arbitrary),
    elements,
    listOf,
    oneof,
  )

instance Arbitrary ConditionalRestriction where
  arbitrary = fmap ConditionalRestriction $ (:) <$> arbitrary <*> (maybeToList <$> arbitrary)

instance Arbitrary Expression where
  arbitrary = Expression <$> elements ["yes", "private", "55 mph", "destination"] <*> listOf arbitrary

instance Arbitrary Condition where
  arbitrary =
    oneof
      [ OH . OpeningHours <$> listOf arbitrary,
        Comparison <$> elements ["wheight", "length", "width", "height", "wheels"] <*> arbitrary <*> arbitrary,
        Absolute <$> elements ["wet", "snow"]
      ]

instance Arbitrary RuleSequence where
  arbitrary = RuleSequence <$> elements [Normal, Additional] <*> arbitrary <*> arbitrary

instance Arbitrary SelectorSequence where
  arbitrary =
    oneof
      [ pure TwentyFourSeven,
        WeekdaySel <$> listOf arbitrary,
        TimeSel <$> listOf arbitrary,
        WeekdayTime <$> listOf arbitrary <*> listOf arbitrary
      ]

instance Arbitrary ComparisonOp where
  arbitrary = elements [Gt, Lt, GtEq, LtEq, Eq]

instance Arbitrary WeekdayRange where
  arbitrary =
    oneof
      [ SingleDay <$> arbitrary,
        WdayRange <$> arbitrary <*> arbitrary
      ]

instance Arbitrary WeekDay where
  arbitrary = elements (enumFromTo Sunday Saturday)

instance Arbitrary TimeSpan where
  arbitrary =
    oneof
      [ Moment <$> arbitrary,
        Span <$> arbitrary <*> arbitrary
      ]

instance Arbitrary TimeOfDay where
  arbitrary = TimeOfDay <$> elements [01 .. 23] <*> elements [00 .. 60] <*> elements [00 .. 60] <*> pure 0
