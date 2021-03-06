module ConditionalRestriction.Parse.OpeningHoursParserSpec where

import ConditionalRestriction (Result (Ok))
import ConditionalRestriction.Internal.Parse.OpeningHoursParser
  ( pOpeningHours,
    pRuleModifier,
    pRuleSequence,
    pSelectorSequence,
    pWeekdaySelector,
  )
import ConditionalRestriction.Parse
  ( Parser (parse),
    pOpeningHours,
  )
import ConditionalRestriction.Parse.AST
  ( OpeningHours (OpeningHours),
    RuleSequence (RuleSequence),
    RuleType (Normal),
    SelectorSequence (TwentyFourSeven, WeekdaySel),
    WeekdayRange (SingleDay, WdayRange),
  )
import Data.Hourglass (WeekDay (Monday, Saturday, Thursday))
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "pOpeningHours" $ do
    it "can parse 'Mo'" $
      parse pOpeningHours "Mo" `shouldBe` Ok (OpeningHours [RuleSequence Normal (WeekdaySel [SingleDay Monday]) (Just True)], "")
  describe "pRuleSequence" $ do
    it "can parse 'Mo'" $
      parse (pRuleSequence Normal) "Mo" `shouldBe` Ok (RuleSequence Normal (WeekdaySel [SingleDay Monday]) (Just True), "")
  describe "pRuleModfier" $ do
    it "can parse 'closed'" $
      parse pRuleModifier "closed" `shouldBe` Ok (Just False, "")
    it "can parse closed plus a comment" $
      parse pRuleModifier "closed \"comment\"" `shouldBe` Ok (Just False, "")
  describe "pSelectorSequence" $ do
    it "can parse '24/7'" $
      parse pSelectorSequence "24/7" `shouldBe` Ok (TwentyFourSeven, "")
  describe "pWeekdaySelector" $ do
    it "can parse 'Mo - Th, Sa'" $
      parse pWeekdaySelector "Mo - Th, Sa" `shouldBe` Ok ([WdayRange Monday Thursday, SingleDay Saturday], "")
