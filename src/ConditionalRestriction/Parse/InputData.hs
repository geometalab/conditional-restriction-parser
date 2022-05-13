module ConditionalRestriction.Parse.InputData where

import Data.Time (UTCTime)
import Data.List (nub)
import Data.Hourglass (DateTime)

type ID = String

data Type
  = TBool
  | TNum
  | TTime
 deriving (Eq, Show)

data Value
  = VBool Bool
  | VNum Double
  | VTime DateTime
 deriving (Eq, Show)
