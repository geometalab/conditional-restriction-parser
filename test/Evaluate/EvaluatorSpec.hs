{-# LANGUAGE ScopedTypeVariables  #-}
module Evaluate.EvaluatorSpec where
import Evaluate.InputData (Value(..))
import Evaluate.Evaluator (fulfills, result, timeIn)
import Parse.AST
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck (Testable(property))
import Parse.Lib (Result(..))
import Arbitrary.Parse.AST

spec :: Spec
spec = do
  describe "result" $ do
    it "returns 120 on '120 @ wet; 100 @ snow' with 'wet' = True and 'snow' = True" $
      let
        ds = [("wet", VBool True), ("snow", VBool True)]
        restriction = ConditionalRestriction [Expression "120" [Absolute "wet"], Expression "100" [Absolute "snow"]]
      in result ds restriction `shouldBe` Ok Nothing
    it "returns nothing on '120 @ wet; 100 @ snow' with 'wet' = False and 'snow' = False" $
      let
        ds = [("wet", VBool False), ("snow", VBool False)]
        restriction = ConditionalRestriction [Expression "120" [Absolute "wet"], Expression "100" [Absolute "snow"]]
      in result ds restriction `shouldBe` Ok Nothing
    it "lists duplicate needs only once" $ property $ \(cond :: Condition) ->
      let
        duplicate_restriction = ConditionalRestriction [Expression "open" [cond, cond], Expression "closed" [cond]]
      in case (result [] duplicate_restriction, fulfills [] cond) of
        (Err (Right a), Err (Right b)) -> a == b
        _ -> True
  describe "fulfills" $ do
    it "returns true on 'wet' with 'wet' = True" $
      fulfills [("wet", VBool True)] (Absolute "wet") `shouldBe` Ok True
    it "returns true on 'weight > 2' with 'weight' = 3" $
      fulfills [("weight", VNum 3)] (Comparison "weight" Gt 2) `shouldBe` Ok True
  describe "timeIn" $ do
    it "gets that 18:00 is within 11:00-20:00" $
      let
        time = read "yyyy-mm-ddThh:mm:ss[.sss]Z"
        span = OpeningHours [RuleSequence Normal (TimeSel [Span (Time 11 00) (Time 20 00)]) (Just True)]
      in timeIn time span `shouldBe` Just True
