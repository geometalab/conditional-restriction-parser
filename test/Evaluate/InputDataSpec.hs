{-# LANGUAGE ScopedTypeVariables  #-}
module Evaluate.InputDataSpec where
import Evaluate.InputData (conditionNeeds, needs)
import Parse.AST (ConditionalRestriction(ConditionalRestriction), Expression (Expression), Condition)
import Test.Hspec (Spec, describe, it)
import Test.QuickCheck (Testable(property))
import Arbitrary.Parse.AST

spec :: Spec
spec = do
  describe "needs" $ do
    it "lists duplicate needs only once" $ property $ \(cond :: Condition) ->
      needs (ConditionalRestriction [Expression "open" [cond, cond], Expression "closed" [cond]]) == conditionNeeds cond
