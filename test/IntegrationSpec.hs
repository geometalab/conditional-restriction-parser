{-# LANGUAGE ScopedTypeVariables #-}

module IntegrationSpec where

import Test.Hspec
import Test.QuickCheck
import Lib (needsData, evaluate)
import Util.Result
import Parse.InputData

spec :: Spec
spec = do
  let term = "no @ (09:00-17:00); destination @ (09:00-17:00 AND disabled)" in describe term $ do
    it "returns 'time' and 'disabled' as needed data" $
      fromResult [] (needsData term) `shouldMatchList` [("time", TTime), ("disabled", TBool)]
    it "returns 'destination' on (time = 2022-05-10 10:00) and (disabled = true)"  $
      evaluate term [("time", "2022-05-10 10:00"), ("disabled", "true")] `shouldBe` Ok(Just "destination")
