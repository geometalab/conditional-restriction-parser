-- | Parsers for input data values.
module ConditionalRestriction.Parse.InputDataParser
  ( pValue,
    pBool,
    pNum,
    pTime,
  )
where

import ConditionalRestriction.Internal.Parse.ParserLib
  ( Parser,
    bint,
    dbl,
    str,
  )
import ConditionalRestriction.Parse.InputData (Value (..))
import Control.Applicative (Alternative ((<|>)))
import Control.Monad (replicateM)
import Data.Hourglass
  ( Date (Date),
    DateTime (DateTime),
    TimeOfDay (TimeOfDay),
  )

-- | Parses 'Value's. See 'pBool', 'pNum' and 'pTime' for formats.
pValue :: Parser String Value
pValue = pBool <|> pTime <|> pNum

-- | Parses boolean values. Possible values are @"true"@ and @"false"@.
pBool :: Parser String Value
pBool =
  VBool True <$ str "true"
    <|> VBool False <$ str "false"

-- | Parses numbers. Values can be with or without decimal places, i.e. @"5"@ or @"5.34"@.
pNum :: Parser String Value
pNum = VNum <$> dbl

-- | Parses time and date in the format @"YYYY-MM-DD hh:mm"@.
pTime :: Parser String Value
pTime = VTime <$> (DateTime <$> p_date <*> (str " " *> p_time))
  where
    p_date = Date <$> p_year <*> (str "-" *> p_month) <*> (str "-" *> p_day)
    p_time = (\h m -> TimeOfDay (fromIntegral h) (fromIntegral m) 0 0) <$> bint 24 <*> (str ":" *> bint 59)
    p_year = bint 9999
    p_month = toEnum . (\x -> x - 1) <$> bint 12
    p_day = bint 31
    concat s c = s ++ [c]
    pack x y = [x, y]
