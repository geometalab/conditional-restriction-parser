module Parse.OpeningHoursParserSpec where

import Test.Hspec (Spec, describe)
import Parse.Parser (pCondition, pMultipleConditions, pExpression, pConditionalRestriction, pCompOperator, pIdentifier)
import Parse.Lib (parse, Result(..))
import Test.Hspec (Spec, describe, it, shouldBe)
import Parse.AST
import Parse.OpeningHoursParser (pOpeningHours, pRuleSequence, pRuleModifier, pSelectorSequence, pWeekdaySelector)

spec :: Spec
spec = do
  describe "pOpeningHours" $ do
    it "can parse 'Mo'" $
      parse pOpeningHours  "Mo" `shouldBe` Ok(OpeningHours [RuleSequence Normal (WeekdaySel [SingleDay Mo]) (Just True)], "")
  describe "pRuleSequence" $ do
    it "can parse 'Mo'" $
      parse (pRuleSequence Normal) "Mo" `shouldBe` Ok(RuleSequence Normal (WeekdaySel [SingleDay Mo]) (Just True), "")
  describe "pRuleModfier" $ do
    it "can parse 'closed'" $
      parse pRuleModifier "closed" `shouldBe` Ok(Just False, "")
    it "can parse closed plus a comment" $
      parse pRuleModifier "closed \"comment\"" `shouldBe` Ok(Just False, "")
  describe "pSelectorSequence" $ do
    it "can parse '24/7'" $
      parse pSelectorSequence "24/7" `shouldBe` Ok(TwentyFourSeven, "")
  describe "pWeekdaySelector" $ do
    it "can parse 'Mo - Th, Sa'" $
      parse pWeekdaySelector "Mo - Th, Sa" `shouldBe` Ok([WdayRange Mo Th, SingleDay Sa], "")