{-|
Input data types and values.
-}
module ConditionalRestriction.Parse.InputData where

import Data.Time (UTCTime)
import Data.List (nub)
import Data.Hourglass (DateTime)

-- | An identifier, identifying a value, e.g. @"weight"@.
type ID = String

-- | Input data type.
data Type
  -- | Boolean type, e.g. value @"true"@.
  = TBool
  -- | Number type, e.g. value @"3.0"@.
  | TNum
  -- | Time type, e.g. value @"2022-05-10 18:00"@.
  | TTime
 deriving (Eq, Show)

-- | Input data value, corresponding to input 'Type's.
data Value
  -- | Boolean value, e.g. @"true"@.
  = VBool Bool
  -- | Number value, e.g. @"3.0"@.
  | VNum Double
  -- | Time value, e.g. @"2022-05-10 18:00"@.
  | VTime DateTime
 deriving (Eq, Show)
