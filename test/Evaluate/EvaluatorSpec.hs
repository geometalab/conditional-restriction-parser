{-# LANGUAGE ScopedTypeVariables  #-}
module Evaluate.EvaluatorSpec where
import Parse.InputData (Value(..))
import Evaluate.Evaluator (fulfills, result, timeIn)
import Parse.AST
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck (Testable(property))
import Parse.Lib
import Util.Result
import Arbitrary.Parse.AST
import Data.Hourglass (WeekDay(Monday, Thursday, Friday, Sunday), DateTime (DateTime), Month (May), Date (Date), TimeOfDay (TimeOfDay))

spec :: Spec
spec = do
  describe "result" $ do
    it "returns 100 on '120 @ wet; 100 @ snow' with 'wet' = True and 'snow' = True" $
      let
        ds = [("wet", VBool True), ("snow", VBool True)]
        restriction = ConditionalRestriction [Expression "120" [Absolute "wet"], Expression "100" [Absolute "snow"]]
      in result ds restriction `shouldBe` Ok (Just "100")
    it "returns nothing on '120 @ wet; 100 @ snow' with 'wet' = False and 'snow' = False" $
      let
        ds = [("wet", VBool False), ("snow", VBool False)]
        restriction = ConditionalRestriction [Expression "120" [Absolute "wet"], Expression "100" [Absolute "snow"]]
      in result ds restriction `shouldBe` Ok Nothing
    it "lists duplicate needs only once" $ property $ \(cond :: Condition) ->
      let
        duplicate_restriction = ConditionalRestriction [Expression "open" [cond, cond], Expression "closed" [cond]]
      in case (result [] duplicate_restriction, fulfills [] cond) of
        (Err (_, [a]), Err (Right b)) -> a == b
        (Err _, _) -> False
        _ -> True
  describe "fulfills" $ do
    it "returns true on 'wet' with 'wet' = True" $
      fulfills [("wet", VBool True)] (Absolute "wet") `shouldBe` Ok True
    it "returns true on 'weight > 2' with 'weight' = 3" $
      fulfills [("weight", VNum 3)] (Comparison "weight" Gt 2) `shouldBe` Ok True
  describe "timeIn" $ do
    it "gets that 18:00 is within 11:00-20:00" $
      let
        time = DateTime (Date 2022 May 10) (TimeOfDay 18 00 00 00)
        span = OpeningHours [RuleSequence Normal (TimeSel [Span (TimeOfDay 11 00 0 0) (TimeOfDay 20 00 0 0)]) (Just True)]
      in timeIn time span `shouldBe` True
    it "gets that 18:00 is not within 11:00-16:00" $
      let
        time = DateTime (Date 2022 May 10) (TimeOfDay 18 00 00 00)
        span = OpeningHours [RuleSequence Normal (TimeSel [Span (TimeOfDay 11 00 0 0) (TimeOfDay 16 00 0 0)]) (Just True)]
      in timeIn time span `shouldBe` False
    it "gets that We is within Mo-Th" $
      let
        time = DateTime (Date 2022 May 11) (TimeOfDay 18 00 00 00) -- wednesday
        span = OpeningHours [RuleSequence Normal (WeekdaySel [WdayRange Monday Thursday]) (Just True)]
      in timeIn time span `shouldBe` True
    it "gets that We is not within Fr-Su" $
      let
        time = DateTime (Date 2022 May 11) (TimeOfDay 18 00 00 00) -- wednesday
        span = OpeningHours [RuleSequence Normal (WeekdaySel [WdayRange Friday Sunday]) (Just True)]
      in timeIn time span `shouldBe` False
    it "can check extended time 11:00-30:00" $
      let
        time = DateTime (Date 2022 May 14) (TimeOfDay 04 00 00 00) -- saturday
        span = OpeningHours [RuleSequence Normal (WeekdayTime [SingleDay Friday] [Span (TimeOfDay 11 00 0 0) (TimeOfDay 30 00 0 0)]) (Just True)]
      in timeIn time span `shouldBe` True
    it "can check extended time 11:00-06:00" $
      let
        time = DateTime (Date 2022 May 14) (TimeOfDay 04 00 00 00) -- saturday
        span = OpeningHours [RuleSequence Normal (WeekdayTime [SingleDay Friday] [Span (TimeOfDay 11 00 0 0) (TimeOfDay 06 00 0 0)]) (Just True)]
      in timeIn time span `shouldBe` True
