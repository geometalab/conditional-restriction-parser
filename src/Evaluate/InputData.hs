module Evaluate.InputData where

import Data.Time (UTCTime)
import Parse.AST (Grammar)

data Type
  = TBool String
  | TNum String
  | TTime String

data Value
  = VBool Bool
  | VNum String
  | VTime UTCTime

dataNeeded :: Grammar -> [Type]
dataNeeded = undefined
