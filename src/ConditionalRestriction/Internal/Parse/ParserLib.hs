{-# LANGUAGE LambdaCase #-}

-- | Parsing library. Implements a simple 'Parser' type, and some basic parsers, e.g. 'ws', 'dbl'.
module ConditionalRestriction.Internal.Parse.ParserLib where

import ConditionalRestriction.Result (Result (..))
import Control.Applicative (Alternative (empty, many, (<|>)))
import Control.Monad (replicateM, (>=>))
import Data.Bifunctor (Bifunctor (first))

-- | A generic parser. Takes an input type @i@ and returns an output type @a@.
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

instance Monad (Parser i) where
  return = pure
  (Parser pa) >>= f = Parser $ \i -> case pa i of
    Ok (a, i') -> parse (f a) i'
    Err msg -> Err msg

str :: String -> Parser String String
str s = Parser $ \i ->
  if take len i == s
    then Ok (s, drop len i)
    else Err $ "Input does not match '" ++ s ++ "': " ++ shorten 16 i
  where
    len = length s

anyOf :: [Char] -> Parser String Char
anyOf cs = Parser $ \case
  [] -> Err "Empty input."
  (i : is) ->
    if i `elem` cs
      then Ok (i, is)
      else Err $ show i ++ " does not match any of '" ++ cs ++ "'"

noneOf :: [Char] -> Parser String Char
noneOf cs = Parser $ \case
  [] -> Err "Empty input."
  (i : is) ->
    if i `elem` cs
      then Err $ show i ++ " matches '" ++ cs ++ "'"
      else Ok (i, is)

ws :: Parser String String
ws = many (anyOf "\t\n ")

word :: String -> Parser String String
word s = str s <* ws

tok :: Parser String String
tok = many (noneOf "\t\n ") <* ws

dbl :: Parser String Double
dbl = Parser $ \i -> case reads i of
  [(x, rem)] -> Ok (x, rem)
  _ -> Err $ "No double: " ++ shorten 16 i

bint :: Int -> Parser String Int
bint max =
  read <$> case digits max of
    [] -> str "0"
    (x : xs) ->
      (:) <$> anyOf ['0' .. d2c (x - 1)] <*> replicateM (length xs) (anyOf ['0' .. '9'])
        <|> (:) <$> anyOf [d2c x] <*> mapM (\x' -> anyOf ['0' .. d2c x']) xs
  where
    digits 0 = []
    digits x = digits (x `div` 10) ++ [x `mod` 10]
    d2c = head . show

end :: Parser String ()
end = Parser $ \case
  [] -> Ok ((), "")
  i -> Err $ "There is still input left: " ++ i

shorten :: Int -> String -> String
shorten len str
  | len > 3 =
    if length str > len
      then take (len - 3) str ++ "..."
      else str
shorten len str = take len str

strip :: String -> String
strip = drop_ws . reverse . drop_ws . reverse
  where
    drop_ws = dropWhile (`elem` "\t\n ")
