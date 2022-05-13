{-# LANGUAGE ScopedTypeVariables  #-}
module ConditionalRestriction.EvaluateSpec where
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck (Testable(property))
import Data.Hourglass (WeekDay(Monday, Thursday, Friday, Sunday, Tuesday, Wednesday), DateTime (DateTime), Month (May), Date (Date), TimeOfDay (TimeOfDay))
import ConditionalRestriction.Internal.Evaluate
import ConditionalRestriction.Result
import ConditionalRestriction.Parse.AST
import ConditionalRestriction.Parse.InputData
import Arbitrary.ConditionalRestriction.Parse.AST
import ConditionalRestriction.Internal.Evaluate (subtractTimespan)

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
    it "gets that 18:00 is within 24/7" $
      let
        time = DateTime (Date 2022 May 10) (TimeOfDay 18 00 00 00)
        span = OpeningHours [RuleSequence Normal TwentyFourSeven (Just True)]
      in timeIn time span `shouldBe` True
    it "gets that 18:00 is not within 24/7 off" $
      let
        time = DateTime (Date 2022 May 10) (TimeOfDay 18 00 00 00)
        span = OpeningHours [RuleSequence Normal TwentyFourSeven (Just False)]
      in timeIn time span `shouldBe` False
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
    it "returns false on 'Mo-Fr; Tue off' for tuesday" $
      let
        time = DateTime (Date 2022 May 10) (TimeOfDay 18 00 00 00) -- tuesday
        span = OpeningHours [RuleSequence Normal (WeekdaySel [WdayRange Monday Friday]) (Just True), RuleSequence Normal (WeekdaySel [SingleDay Tuesday]) (Just False)]
      in timeIn time span `shouldBe` False
    it "returns true on 'Mo-Fr off; Tue' for tuesday" $
      let
        time = DateTime (Date 2022 May 10) (TimeOfDay 18 00 00 00) -- tuesday
        span = OpeningHours [RuleSequence Normal (WeekdaySel [WdayRange Monday Friday]) (Just False), RuleSequence Normal (WeekdaySel [SingleDay Tuesday]) (Just True)]
      in timeIn time span `shouldBe` True
  describe "ohTimes" $ do
    it "returns only Mo and Mi for 'Mo-We; Tue off'" $
      let
        min = TimeOfDay 00 00 00 0
        max = TimeOfDay 23 59 59 999999999
        span = OpeningHours [RuleSequence Normal (WeekdaySel [WdayRange Monday Wednesday]) (Just True), RuleSequence Normal (WeekdaySel [SingleDay Tuesday]) (Just False)]
      in ohTimes span `shouldBe` [[], [Span min max], [], [Span min max], [], [], []]
    it "returns Mo, Tu, Mi for 'Tue off; Mo-We'" $
      let
        min = TimeOfDay 00 00 00 0
        max = TimeOfDay 23 59 59 999999999
        span = OpeningHours [RuleSequence Normal (WeekdaySel [SingleDay Tuesday]) (Just False), RuleSequence Normal (WeekdaySel [WdayRange Monday Wednesday]) (Just True)]
      in ohTimes span `shouldBe` [[], [Span min max], [Span min max], [Span min max], [], [], []]
  describe "addTimespan" $ do
    it "is commutative" $ property $ \ts1 ts2 t -> timeInSelector t (addTimespan ts1 [ts2]) == timeInSelector t (addTimespan ts2 [ts1])
  describe "subtractTimespan" $ do
    it "is undone by addTimespan" $ property $ \ts1 ts2 t -> timeInSelector t (addTimespan ts1 (subtractTimespan ts1 [ts2])) == timeInSelector t (addTimespan ts1 [ts2])
