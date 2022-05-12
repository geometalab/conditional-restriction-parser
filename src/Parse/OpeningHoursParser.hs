module Parse.OpeningHoursParser where
import Parse.Lib
import Parse.AST
import Control.Applicative (Alternative(many, (<|>)), optional)
import Data.Hourglass (WeekDay(..), TimeOfDay (TimeOfDay))

pOpeningHours :: Parser String OpeningHours
pOpeningHours = OpeningHours <$> ((:) <$> pRuleSequence Normal <*> many next_rule_sequence)
  where next_rule_sequence = word ";" *> pRuleSequence Normal
                         <|> word "," *> pRuleSequence Additional

pRuleSequence :: RuleType -> Parser String RuleSequence
pRuleSequence t = RuleSequence t <$> pSelectorSequence <*> pRuleModifier

pRuleModifier :: Parser String OHState
pRuleModifier = (Just False <$ (word "closed" <|> word "off")
            <|> Nothing <$ word "unknown"
            <|> Just True <$ (word "open" <|> ws)) <* optional pComment

pSelectorSequence :: Parser String SelectorSequence
pSelectorSequence = TwentyFourSeven <$ word "24/7"
                <|> (WeekdayTime <$> pWeekdaySelector <*> pTimeSelector)
                <|> (WeekdaySel <$> pWeekdaySelector)
                <|> (TimeSel <$> pTimeSelector)

pWeekdaySelector :: Parser String WeekdaySelector
pWeekdaySelector = (:) <$> pWeekdayRange <*> many (word "," *> pWeekdayRange)

pTimeSelector :: Parser String TimeSelector
pTimeSelector = (:) <$> pTimeSpan <*> many (word "," *> pTimeSpan)

pWeekdayRange :: Parser String WeekdayRange
pWeekdayRange = WdayRange <$> pWday <*> (word "-" *> pWday)
            <|> SingleDay <$> pWday

pWday :: Parser String WeekDay
pWday = Sunday <$ word "Su"
    <|> Monday <$ word "Mo"
    <|> Tuesday <$ word "Tu"
    <|> Wednesday <$ word "We"
    <|> Thursday <$ word "Th"
    <|> Friday <$ word "Fr"
    <|> Saturday <$ word "Sa"

pTimeSpan :: Parser String TimeSpan
pTimeSpan = (Span <$> pTime False <*> (word "-" *> pTime True))
        <|> Moment <$> pTime False

pTime :: Bool -> Parser String TimeOfDay
pTime extended = (\h m -> TimeOfDay (fromIntegral h) (fromIntegral m) 0 0) <$> p_hour <*> pMinute
  where
    p_hour = if extended then pExtendedHour else pHour

pHour :: Parser String Int
pHour = read <$> (p0 <|> p1 <|> p2)
  where
    concat s c = s ++ [c]
    p0 = concat <$> str "0" <*> anyOf ['0'..'9']
    p1 = concat <$> str "1" <*> anyOf ['0'..'9']
    p2 = concat <$> str "2" <*> anyOf ['0'..'4']

pExtendedHour :: Parser String Int
pExtendedHour = read <$> (p03 <|> p4)
  where
    p03 = (\x y -> [x,y]) <$> anyOf ['0'..'3'] <*> anyOf ['0'..'9']
    p4 = (\s c -> s ++ [c]) <$> str "4" <*> anyOf ['0'..'8']

pMinute :: Parser String Int
pMinute = read <$> ((\x y -> [x, y]) <$> anyOf ['0'..'5'] <*> anyOf ['0'..'9'])

pComment :: Parser String String
pComment = str "\"" *> many (noneOf "\"") <* word "\""
