{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}

module ConditionalRestrictionSpec where

import ConditionalRestriction
  ( Result (Err, Ok),
    Type (TBool, TTime),
    evaluate,
    needsData,
  )
import ConditionalRestriction.Result (Result (Ok), fromResult)
import Test.Hspec (Spec, describe, it, shouldBe, shouldMatchList, shouldSatisfy)

spec :: Spec
spec = do
  describe "needsData" $ do
    it "returns an empty list if no information is required" $
      needsData "no @ 24/7" `shouldBe` Ok []
  describe "evaluate" $ do
    it "returns an error message when there is a syntax error in the restriction" $
      evaluate "no (at) 17:00-18:00" [] `shouldSatisfy` \case
        Err ([!_], []) -> True
        _ -> False
    it "returns an error message when there is a syntax error in the data" $
      evaluate "no @ 17:00-18:00" [("time", "2022-05-20 10::15")] `shouldSatisfy` \case
        Err ([!_], []) -> True
        _ -> False
