{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ConditionalRestriction.Parse.ParserLibSpec where

-- cannot import Fn specifically

import ConditionalRestriction (Result (..))
import ConditionalRestriction.Internal.Parse.ParserLib
  ( Parser (parse),
    anyOf,
    bint,
    dbl,
    noneOf,
    shorten,
    str,
    strip,
    tok,
    word,
    ws,
  )
import Control.Applicative (Alternative (empty, (<|>)))
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import Test.QuickCheck (property)
import Test.QuickCheck.Function -- cannot import Fn directly

{-# ANN spec "HLint: ignore Use <$>" #-} -- L31, ignore because <*> is the function we are testing, so we don't want to replace it with <$>
{-# ANN spec "HLint: ignore Alternative law, left identity" #-} -- L48, ignore because we are checking left identity
spec :: Spec
spec = do
  describe "Show (Result e)" $ do
    describe "show" $ do
      it "shows Ok 'test' as Ok 'test'" $ show (Ok "test" :: Result String String) `shouldBe` "Ok \"test\""
  describe "Applicative (Result e)" $ do
    describe "pure" $ do
      it "is the same as Ok" $ property $ \(x :: String) -> pure x == (Ok x :: Result String String)
    describe "(<*>)" $ do
      it "can apply any function" $ property $ \(Fn (f :: String -> String)) (x :: String) -> (pure f <*> pure x) == (pure (f x) :: Result String String)
  describe "Monad (Result e)" $ do
    describe "return" $ do
      it "is the same as pure" $ property $ \(x :: String) -> return x == (pure x :: Result String String)
  describe "Alternative (Parser i)" $ do
    describe "empty" $ do
      it "always throws an error" $
        property $ \i -> case parse (empty :: Parser String String) i of
          Err _ -> True
          Ok _ -> False
    describe "(<|>)" $ do
      it "Chooses second option if first is erroneous" $ property $ \i -> parse (empty <|> str "") i == parse (str "") i
  describe "Monad (Parser i)" $ do
    describe "return" $ do
      it "behaves the same as pure" $ property $ \(x :: String) (i :: String) -> parse (pure x) i == parse (return x) i
    describe "(>>=)" $ do
      it "parses the same as with applicatives" $
        property $ \(x :: String) i ->
          parse ((,) <$> str x <*> str x) i
            == parse
              ( do
                  a <- str x
                  b <- str x
                  return (a, b)
              )
              i
  describe "str" $ do
    it "can parse any given string" $ property $ \(a, b) -> parse (str a) (a ++ b) == Ok (a, b)
  describe "anyOf" $ do
    it "can parse the char if it is in the list" $
      property $ \c s ->
        let shouldParse = c `elem` s
         in case parse (anyOf s) [c] of
              Err _ | not shouldParse -> True
              Ok (_, "") | shouldParse -> True
              _ -> False
  describe "noneOf" $ do
    it "can parse the char if it is not in the list" $
      property $ \c s ->
        let should_parse = c `notElem` s
         in case parse (noneOf s) [c] of
              Err _ | not should_parse -> True
              Ok (_, "") | should_parse -> True
              _ -> False
  describe "ws" $ do
    it "can parse a string containing only tabs, spaces and newlines" $
      let s = "\t \n "
       in parse ws s `shouldBe` Ok (s, "")
    it "can parse an empty string" $
      parse ws "" `shouldBe` Ok ("", "")
    it "never outputs a result other than whitespace" $
      property $ \s ->
        let contains_other = any (`notElem` "\t\n ")
         in case parse ws s of
              Err _ -> True
              Ok (res, _) -> not $ contains_other res
  describe "word" $ do
    it "can parse any given string followed by whitespace" $
      property $ \(a, b) ->
        case parse (word a) (a ++ " " ++ b) of
          Ok (a', _) -> a == a'
          _ -> False
  describe "tok" $ do
    it "can parse 'a b'" $ parse tok "a b" `shouldBe` Ok ("a", "b")
  describe "dbl" $ do
    it "can parse any double" $
      property $ \(d :: Double) ->
        parse dbl (show d) == Ok (d, "")
    it "throws an error on invalid value 'a'" $
      parse dbl "a" `shouldSatisfy` \case
        Err !_ -> True
        _ -> False
  describe "bint" $ do
    it "can parse its max value" $ property $ \i -> let i' = abs i in parse (bint i') (show i') == Ok (i', "")
  describe "shorten" $ do
    it "never outputs a string longer than given length" $
      property $ \len s ->
        length (shorten (abs len) s) <= abs len
  describe "strip" $ do
    it "never outputs a string starting with whitespace" $
      property $ \s -> case strip s of
        (c : cs) | c `elem` "\t\n " -> False
        _ -> True
    it "never outputs a string ending with whitespace" $
      property $ \s -> case reverse $ strip s of
        (c : cs) | c `elem` "\t\n " -> False
        _ -> True
