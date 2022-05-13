module ConditionalRestriction.Parse.InputDataParserSpec where

import ConditionalRestriction.Parse
  ( Parser (parse),
    Value (VBool, VNum, VTime),
    pValue,
  )
import ConditionalRestriction.Parse.InputData
  ( Value (VBool, VNum, VTime),
  )
import ConditionalRestriction.Parse.InputDataParser (pValue)
import ConditionalRestriction.Result (Result (Ok))
import Data.Hourglass
  ( Date (Date),
    DateTime (DateTime),
    Month (May),
    TimeOfDay (TimeOfDay),
  )
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "pValue" $ do
    it "can parse a bool" $ parse pValue "true" `shouldBe` Ok (VBool True, "")
    it "can parse a number" $ parse pValue "2.0" `shouldBe` Ok (VNum 2, "")
    it "can parse a time" $ parse pValue "2022-05-10 12:23" `shouldBe` Ok (VTime $ DateTime (Date 2022 May 10) (TimeOfDay 12 23 0 0), "")
