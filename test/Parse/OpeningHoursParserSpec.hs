module Parse.OpeningHoursParserSpec where

import Test.Hspec (Spec, describe)
import Parse.Parser (pCondition, pMultipleConditions, pExpression, pConditionalRestriction, pCompOperator, pIdentifier)
import Parse.Lib (parse, Result(..))
import Test.Hspec (Spec, describe, it, shouldBe)
import Parse.AST
import Parse.OpeningHoursParser (pOpeningHours)

spec :: Spec
spec = do
  describe "pOpeningHours" $ do
    it "can parse 'Mo'" $
      parse pOpeningHours  "Mo" `shouldBe` Ok(OpeningHours [RuleSequence Normal (WeekdaySel [SingleDay Mo]) (Just True)], "")
