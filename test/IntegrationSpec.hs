{-# LANGUAGE ScopedTypeVariables #-}

module IntegrationSpec where

import Lib (add, sub)
import Test.Hspec (Spec, describe, it)
import Test.QuickCheck

spec :: Spec
spec = do
  describe "src/Lib.hs" $ do
    describe "add" $ do
      it "adds numbers" $ property $ \(a :: Int) (b :: Int) -> add a b == a + b
    describe "sub" $ do
      it "subtracts numbers" $ property $ \(a :: Int) (b :: Int) -> sub a b == a - b
