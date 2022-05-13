module Arbitrary.ConditionalRestriction.Parse.InputData where

import ConditionalRestriction.Parse.InputData (Type (..))
import Test.QuickCheck (Arbitrary (arbitrary), elements)

instance Arbitrary Type where
  arbitrary = elements [TBool, TNum, TTime]
