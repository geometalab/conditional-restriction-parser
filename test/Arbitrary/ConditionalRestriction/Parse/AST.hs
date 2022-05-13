module Arbitrary.ConditionalRestriction.Parse.AST where

import ConditionalRestriction.Parse.AST
  ( ComparisonOp (..),
    Condition (..),
    ConditionalRestriction (..),
    Expression (..),
    TimeSpan (..),
  )
import Data.Hourglass (TimeOfDay (TimeOfDay))
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
      [ pure $ OH undefined,
        Comparison <$> elements ["wheight", "length", "width", "height", "wheels"] <*> arbitrary <*> arbitrary,
        Absolute <$> elements ["wet", "snow"]
      ]

instance Arbitrary ComparisonOp where
  arbitrary = elements [Gt, Lt, GtEq, LtEq, Eq]

instance Arbitrary TimeSpan where
  arbitrary =
    oneof
      [ Moment <$> arbitrary,
        Span <$> arbitrary <*> arbitrary
      ]

instance Arbitrary TimeOfDay where
  arbitrary = TimeOfDay <$> elements [01 .. 23] <*> elements [00 .. 60] <*> elements [00 .. 60] <*> pure 0
