module Parse.InputDataParserSpec where

import Test.Hspec
import Parse.InputDataParser (pValue)
import Parse.InputData
import Data.Hourglass (TimeOfDay(TimeOfDay), Date (Date), DateTime (DateTime), Month (May))
import Parse.Lib
import Util.Result

spec :: Spec
spec = do
  describe "pValue" $ do
    it "can parse a bool" $ parse pValue "true" `shouldBe` Ok(VBool True, "")
    it "can parse a number" $ parse pValue "2.0" `shouldBe` Ok(VNum 2, "")
    it "can parse a time" $ parse pValue "2022-05-10 12:23" `shouldBe` Ok(VTime $ DateTime (Date 2022 May 10) (TimeOfDay 12 23 0 0), "")
