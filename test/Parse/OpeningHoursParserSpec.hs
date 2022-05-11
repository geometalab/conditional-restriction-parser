module Parse.OpeningHoursParserSpec where

import Test.Hspec (Spec, describe)
import Parse.Parser (pCondition, pMultipleConditions, pExpression, pConditionalRestriction, pCompOperator, pIdentifier)
import Parse.Lib (parse, Result(..))
import Test.Hspec (Spec, describe, it, shouldBe)
import Parse.AST (ComparisonOp(..), Condition(..), Expression (Expression), ConditionalRestriction (ConditionalRestriction), OpeningHours (OpeningHours), RuleSequence (RuleSequence), SelectorSequence (WeekdaySel), WeekdayRange (SingleDay), Wday (Mo))
import Parse.OpeningHoursParser (pOpeningHours)

spec :: Spec
spec = do
  describe "pOpeningHours" $ do
    it "can parse 'Mo'" $
      parse pOpeningHours  "Mo" `shouldBe` Ok(OpeningHours [RuleSequence (WeekdaySel [SingleDay Mo]) (Just True)], "")
