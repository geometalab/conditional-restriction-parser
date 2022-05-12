module Parse.InputDataParser where
import Parse.Lib
import Parse.AST
import Control.Applicative (Alternative(many, (<|>)), optional)
import Parse.InputData
import Data.Hourglass (TimeOfDay(TimeOfDay), Date (Date), DateTime (DateTime))
import Control.Monad (replicateM)

pValue :: Parser String Value
pValue = pBool <|> pTime <|> pNum

pBool :: Parser String Value
pBool = VBool True <$ str "true"
    <|> VBool False <$ str "false"

pNum :: Parser String Value
pNum = VNum <$> dbl

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
