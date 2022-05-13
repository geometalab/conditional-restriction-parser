module Arbitrary.ConditionalRestriction.Parse.InputData where
import Test.QuickCheck (Arbitrary (arbitrary), elements)
import ConditionalRestriction.Parse.InputData (Type (TBool, TNum, TTime), Value)

instance Arbitrary Type where
  arbitrary = elements [TBool, TNum, TTime]
