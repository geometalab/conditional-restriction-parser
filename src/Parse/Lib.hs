{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Parse.Lib where

import Control.Applicative (Alternative (many), empty, (<|>))
import Control.Monad ((>=>))
import Data.Bifunctor (Bifunctor (first))

data Result e a
  = Err e
  | Ok a
  deriving (Eq, Show)

instance Functor (Result e) where
  fmap f (Ok a) = Ok (f a)
  fmap _ (Err e) = Err e

instance Applicative (Result e) where
  pure = Ok
  (Ok f) <*> (Ok x) = Ok (f x)
  (Err f) <*> _ = Err f
  _ <*> (Err x) = Err x

instance Monad (Result e) where
  return = pure
  (Ok x) >>= f = f x
  (Err x) >>= _ = Err x

newtype Parser i a = Parser
  { parse :: i -> Result String (a, i)
  }

instance Functor (Parser i) where
  fmap f (Parser p) = Parser $ fmap (first f) . p

instance Applicative (Parser i) where
  pure x = Parser $ \i -> Ok (x, i)
  (Parser pf) <*> (Parser px) = Parser $ pf >=> \(f, i') -> first f <$> px i'

instance Alternative (Parser i) where
  empty = Parser $ \i -> Err "No parsing possibilities left."
  (Parser a) <|> (Parser b) = Parser $ \i -> case a i of
    res@(Ok _) -> res
    Err msg -> b i

str :: String -> Parser String String
str s = Parser $ \i ->
  if take len i == s
    then Ok (s, drop len i)
    else Err $ "Input does not match '" ++ s ++ "': " ++ shorten 16 i
  where
    len = length s

anyOf :: [Char] -> Parser String Char
anyOf cs = Parser $ \(i : is) ->
  if i `elem` cs
    then Ok (i, is)
    else Err $ show i ++ " does not match any of '" ++ cs ++ "'"

noneOf :: [Char] -> Parser String Char
noneOf cs = Parser $ \(i : is) ->
  if i `elem` cs
    then Err $ show i ++ " matches '" ++ cs ++ "'"
    else Ok (i, is)

ws :: Parser String String
ws = many (anyOf "\t\n ")

shorten :: Int -> String -> String
shorten len str =
  if length str > len
    then take (len - 3) str ++ "..."
    else str
