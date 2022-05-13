module ConditionalRestriction.Parse.InputDataParser where

import Control.Applicative (Alternative(many, (<|>)), optional)
import Data.Hourglass (TimeOfDay(TimeOfDay), Date (Date), DateTime (DateTime))
import Control.Monad (replicateM)
import ConditionalRestriction.Parse.ParserLib
import ConditionalRestriction.Parse.InputData

-- | parses 'Value's. See 'pBool', 'pNum' and 'pTime' for formats.
pValue :: Parser String Value
pValue = pBool <|> pTime <|> pNum

-- | parses boolean values. Possible values are @"true"@ and @"false"@.
pBool :: Parser String Value
pBool = VBool True <$ str "true"
    <|> VBool False <$ str "false"

-- | parses numbers. Values can be with or without decimal places, i.e. @"5"@ or @"5.34"@.
pNum :: Parser String Value
pNum = VNum <$> dbl

-- | parses time and date in the format @"YYYY-MM-DD hh:mm"@.
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
