{-# LANGUAGE ScopedTypeVariables #-}

module IntegrationSpec where

import Test.Hspec
import Test.QuickCheck
import ConditionalRestriction
import ConditionalRestriction.Result

spec :: Spec
spec = do
  let term = "no @ (09:00-17:00); destination @ (09:00-17:00 AND disabled)" in describe term $ do
    it "returns 'time' and 'disabled' as needed data" $
      fromResult [] (needsData term) `shouldMatchList` [("time", TTime), ("disabled", TBool)]
    it "returns 'destination' on (time = 2022-05-10 10:00) and (disabled = true)"  $
      evaluate term [("time", "2022-05-10 10:00"), ("disabled", "true")] `shouldBe` Ok (Just "destination")
    it "returns unknown on (time = 2022-05-10 08:00) and (disabled = true)"  $
      evaluate term [("time", "2022-05-10 08:00"), ("disabled", "true")] `shouldBe` Ok Nothing
  let term = "120 @ (06:00-20:00); 80 @ wet" in describe term $ do
    it "returns 80 on (time = 2022-05-10 07:00) and (wet = true)" $
      evaluate term [("time", "2022-05-10 07:00"), ("wet", "true")] `shouldBe` Ok (Just "80")
    it "returns 120 on (time = 2022-05-10 07:00) and (wet = false)" $
      evaluate term [("time", "2022-05-10 07:00"), ("wet", "false")] `shouldBe` Ok (Just "120")
  let term = "delivery @ (07:00-11:00); customers @ (07:00-17:00)" in describe term $ do
    it "returns 'customers' on (time = 2022-05-10 07:00)" $
      evaluate term [("time", "2022-05-10 07:00")] `shouldBe` Ok (Just "customers")
    it "returns unknown on (time = 2022-05-10 06:00)" $
      evaluate term [("time", "2022-05-10 06:00")] `shouldBe` Ok Nothing
  let term = "foo @ (24/7); bar @ (Mo-Sa 10:00-12:00,13:00-20:00; Tu off); baz @ (Su-Tu 11:00-01:00, We-Th 11:00-03:00, Fr 11:00-06:00, Sa 11:00-07:00)" in describe term $ do
    it "returns 'foo' on (time = 2022-05-10 10:00 (Tuesday))" $
      evaluate term [("time", "2022-05-10 10:00")] `shouldBe` Ok (Just "foo")
    it "returns 'baz' on (time = 2022-05-10 11:00 (Tuesday))" $
      evaluate term [("time", "2022-05-10 11:00")] `shouldBe` Ok (Just "baz")
    it "returns 'bar' on (time = 2022-05-09 10:30 (Monday))" $
      evaluate term [("time", "2022-05-09 10:30")] `shouldBe` Ok (Just "bar")
