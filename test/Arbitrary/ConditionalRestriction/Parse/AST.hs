module Arbitrary.ConditionalRestriction.Parse.AST where
import Test.QuickCheck (Arbitrary (arbitrary), elements, listOf, oneof, Gen)
import ConditionalRestriction.Parse.AST (ConditionalRestriction (ConditionalRestriction), Condition (..), ComparisonOp (..), Expression (Expression))
import Data.Maybe (fromMaybe, maybeToList)

instance Arbitrary ConditionalRestriction where
  arbitrary = fmap ConditionalRestriction $ (:) <$> arbitrary <*> (maybeToList <$> arbitrary)

instance Arbitrary Expression where
  arbitrary = Expression <$> elements ["yes", "private", "55 mph", "destination"] <*> listOf arbitrary

instance Arbitrary Condition where
  arbitrary = oneof
    [ pure $ OH undefined
    , Comparison <$> elements ["wheight", "length", "width", "height", "wheels"] <*> arbitrary <*> arbitrary
    , Absolute <$> elements ["wet", "snow"]
    ]

instance Arbitrary ComparisonOp where
  arbitrary = elements [Gt, Lt, GtEq, LtEq, Eq]
