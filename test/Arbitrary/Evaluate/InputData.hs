module Arbitrary.Evaluate.InputData where
import Test.QuickCheck (Arbitrary (arbitrary), elements)
import Evaluate.InputData (Type (TBool, TNum, TTime), Value)

instance Arbitrary Type where
  arbitrary = elements [TBool, TNum, TTime]
