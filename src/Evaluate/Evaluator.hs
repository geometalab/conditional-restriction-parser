{-# LANGUAGE LambdaCase #-}
module Evaluate.Evaluator where

import Evaluate.InputData (Type (..), Value (..), ID)
import Parse.AST
import Parse.Lib (Result(..))
import Data.Time (UTCTime)
import Util.Monad (findM, allM)

type EvalError = Either String [(ID, Type)]

result :: [(ID, Value)] -> ConditionalRestriction -> Result EvalError (Maybe Token) -- TODO combine needed data output
result ds (ConditionalRestriction exprs) = findM (\(Expression _ conds) -> allM (fulfills ds) conds) exprs >>= \case
  Nothing -> Ok Nothing
  Just (Expression tok _) -> Ok $ Just tok

fulfills :: [(ID, Value)] -> Condition -> Result EvalError Bool
fulfills ds (OH _) = undefined
fulfills ds (Comparison tok op val) = case lookup tok ds of
  Just (VNum d) -> Ok $ case op of
    Gt -> d > val
    Lt -> d < val
    GtEq -> d >= val
    LtEq -> d <= val
    Eq -> d == val -- TODO is it safe to compare doubles directly here?
  Just _ -> Err . Left $ "Incorrect input type for " ++ tok
  Nothing -> Err $ Right [(tok, TNum)]
fulfills ds (Absolute tok) = case lookup tok ds of
  Just (VBool b) -> Ok b
  Just _ -> Err . Left $ "Incorrect input type for " ++ tok
  Nothing -> Err $ Right [(tok, TBool)]


timeIn :: UTCTime -> OpeningHours -> Maybe Bool
timeIn = undefined
