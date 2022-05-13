{-# LANGUAGE TupleSections #-}
module ConditionalRestriction
  ( needsData,
    evaluate,
    ID, Value(..), Type(..), Token,
    Result(..)
  )
where

import Data.Bifunctor (Bifunctor(first))
import ConditionalRestriction.Result
import ConditionalRestriction.Parse
import ConditionalRestriction.Evaluate
import ConditionalRestriction.Parse.ParserLib (end)


needsData :: String -> Result String [(ID, Type)]
needsData s = parse (pConditionalRestriction <* end) s >>= \(r, _) -> case result [] r of
  Err (_, neededs) -> Ok neededs
  Ok _ -> Ok []


evaluate :: String -> [(ID, String)] -> Result ([String], [(ID, Type)]) (Maybe Token)
evaluate s ds = do
  (r, _) <- first (\msg -> (["Parser error in restriction: " ++ msg], [])) $ parse (pConditionalRestriction  <* end) s
  ds' <- first (\msg -> (["Parser error in data: " ++ msg], [])) $ mapM (\(id, d) -> (id,) . fst <$> parse (pValue <* end) d) ds
  result ds' r
