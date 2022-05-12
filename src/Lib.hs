{-# LANGUAGE TupleSections #-}
module Lib
  ( needsData,
    evaluate,
    timeIn,
    timeInSelector
  )
where

import Parse.Parser (pConditionalRestriction)
import Parse.InputData ( ID, Type, Value)
import Parse.AST (Token)
import Util.Result
import Evaluate.Evaluator (result, timeIn, timeInSelector)
import Parse.Lib (Parser(parse), end)
import Parse.InputDataParser (pValue)
import Data.Bifunctor (Bifunctor(first))


needsData :: String -> Result String [(ID, Type)]
needsData s = parse (pConditionalRestriction <* end) s >>= \(r, _) -> case result [] r of
  Err (_, neededs) -> Ok neededs
  Ok _ -> Ok []


evaluate :: String -> [(ID, String)] -> Result ([String], [(ID, Type)]) (Maybe Token)
evaluate s ds = do
  (r, _) <- first (\msg -> (["Parser error in restriction: " ++ msg], [])) $ parse (pConditionalRestriction  <* end) s
  ds' <- first (\msg -> (["Parser error in data: " ++ msg], [])) $ mapM (\(id, d) -> (id,) . fst <$> parse (pValue <* end) d) ds
  result ds' r
