{-# LANGUAGE TupleSections #-}
{-|
The ConditionalRestriction library offers functionality for parsing and
evaluating of conditional restriction values
(see [OSM Wiki](https://wiki.openstreetmap.org/wiki/Conditional_restrictions)).

This module offers functions suitable for most basic use cases.
-}
module ConditionalRestriction
  ( needsData,
    evaluate,
    parseRestriction,
    ID, Value(..), Type(..), Token,
    Result(..)
  )
where

import Data.Bifunctor (Bifunctor(first))
import ConditionalRestriction.Result
import ConditionalRestriction.Parse
import ConditionalRestriction.Evaluate
import ConditionalRestriction.Internal.Parse.ParserLib (parse, end)


-- | Takes a conditional restriction string and returns the data needed in order to evaluate this string.
-- If the conditional restriction couldn't be parsed,
-- an error message is returned instead.
needsData :: String -> Result String [(ID, Type)]
needsData s = parseRestriction s >>= \r -> case result [] r of
  Err (_, neededs) -> Ok neededs
  Ok _ -> Ok []

-- | Takes a conditional restriction string and some input data.
-- It returns the value as a token if any restriction condition was met, or 'Nothing' otherwise.
-- If there was a parsing error or a problem with the provided data, a list of error messages
-- and a list of needed data is returned.
evaluate :: String -> [(ID, String)] -> Result ([String], [(ID, Type)]) (Maybe Token)
evaluate s ds = do
  r <- first (\msg -> (["Parser error in restriction: " ++ msg], [])) $ parseRestriction s
  ds' <- first (\msg -> (["Parser error in data: " ++ msg], [])) $ mapM (\(id, d) -> (id,) . fst <$> parse (pValue <* end) d) ds
  result ds' r

-- | Takes a conditional restriction string and returns the corresponding AST.
-- Take a look at the "ConditionalRestriction.Parse.AST" module for AST manipulation.
parseRestriction :: String -> Result String ConditionalRestriction
parseRestriction = fmap fst . parse (pConditionalRestriction  <* end)
