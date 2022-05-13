{-|
Parser for opening hours (incomplete).
-}
module ConditionalRestriction.Internal.Parse.OpeningHoursParser where

import Control.Applicative (Alternative(many, (<|>)), optional)
import Data.Hourglass (WeekDay(..), TimeOfDay (TimeOfDay))
import ConditionalRestriction.Internal.Parse.ParserLib
import ConditionalRestriction.Parse.AST
    ( TimeSpan(..),
      WeekdayRange(..),
      SelectorSequence(..),
      TimeSelector,
      WeekdaySelector,
      RuleSequence(..),
      RuleType(..),
      OHState,
      OpeningHours(..) )

-- | Parse opening hours, e.g. @"Di-Fr 08:00-20:00"@.
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
pTime extended = (\h m -> TimeOfDay (fromIntegral h) (fromIntegral m) 0 0) <$> p_hour <*> (str ":" *> bint 59)
  where
    p_hour = if extended then bint 48 else bint 24


pComment :: Parser String String
pComment = str "\"" *> many (noneOf "\"") <* word "\""
