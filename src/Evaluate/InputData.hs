module Evaluate.InputData where

import Data.Time (UTCTime)
import Parse.AST (Condition (..), ConditionalRestriction (ConditionalRestriction), Expression (Expression))
import Data.List (nub)

type ID = String

data Type
  = TBool
  | TNum
  | TTime
 deriving (Eq, Show)

data Value
  = VBool Bool
  | VNum Double
  | VTime UTCTime
 deriving (Eq, Show)

-- needs :: ConditionalRestriction -> [(ID, Type)]
-- needs (ConditionalRestriction exprs) = nub [ t | (Expression _ conds) <- exprs, cond <- conds, t <- conditionNeeds cond ]

-- conditionNeeds :: Condition -> [(ID, Type)]
-- conditionNeeds (OpeningHours _) = [("now", TTime)]
-- conditionNeeds (Comparison tok _ val) = [(tok, TNum)]
-- conditionNeeds (Absolute tok) = [(tok, TBool)]
