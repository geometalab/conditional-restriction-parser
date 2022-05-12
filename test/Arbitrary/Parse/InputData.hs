module Arbitrary.Parse.InputData where
import Test.QuickCheck (Arbitrary (arbitrary), elements)
import Parse.InputData (Type (TBool, TNum, TTime), Value)

instance Arbitrary Type where
  arbitrary = elements [TBool, TNum, TTime]
