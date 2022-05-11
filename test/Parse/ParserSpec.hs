module Parse.ParserSpec where

import Test.Hspec (Spec, describe)
import Parse.Parser (pCondition, pMultipleConditions, pExpression, pConditionalRestriction, pCompOperator, pIdentifier)
import Parse.Lib (parse, Result(..))
import Test.Hspec (Spec, describe, it, shouldBe)
import Parse.AST (ComparisonOp(..), Condition(..), Expression (Expression), ConditionalRestriction (ConditionalRestriction))

spec :: Spec
spec = do
  describe "pConditionalRestriction" $ do
    it "can parse '120 @ wet'" $
      parse pConditionalRestriction  "120 @ wet" `shouldBe` Ok(ConditionalRestriction [Expression "120" [Absolute "wet"]], "")
    it "can parse '120 @ wet; 100 @ snow'" $
      parse pConditionalRestriction  "120 @ wet; 100 @ snow" `shouldBe`
        Ok(ConditionalRestriction [Expression "120" [Absolute "wet"], Expression "100" [Absolute "snow"]], "")
  describe "pExpression" $ do
    it "can parse '120 @ wet'" $
      parse pExpression "120 @ wet" `shouldBe` Ok(Expression "120" [Absolute "wet"], "")
  describe "pMultipleConditions" $ do
    it "can parse a single condition" $
      parse pMultipleConditions "weight > 4.5" `shouldBe` Ok([Comparison "weight" Gt 4.5], "")
    it "can parse two conditions" $
      parse pMultipleConditions "weight > 4.5 AND wet" `shouldBe` Ok([Comparison "weight" Gt 4.5, Absolute "wet"], "")
  describe "pCondition" $ do
    it "can parse a wheight comparison" $ parse pCondition "weight > 4.5" `shouldBe` Ok(Comparison "weight" Gt 4.5, "")
  describe "pCompOperator" $ do
    it "can parse '>='" $ parse pCompOperator ">=" `shouldBe` Ok(GtEq, "")
  describe "pIdentifier" $ do
    it "accepts 'test-value' as an identifier" $ parse pIdentifier "test-value" `shouldBe` Ok("test-value", "")
    it "does not accept 'test-value-1' as an identifier" $ parse pIdentifier "test-value-1" `shouldBe` Ok("test-value-", "1")
