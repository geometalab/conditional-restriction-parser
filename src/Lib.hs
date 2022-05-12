module Lib
  ( needsData,
    evaluate,
  )
where

import Parse.Parser (pConditionalRestriction)
import Parse.InputData ( ID, Type, Value)
import Parse.Lib (Result)
import Parse.AST (Token)


needsData :: String -> Result String [(ID, Type)]
needsData = undefined

evaluate :: String -> [(ID, Value)] -> Result String (Maybe Token)
evaluate = undefined
