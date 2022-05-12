module Main where

import Lib
import Evaluate.Evaluator (timeIn, ohTimes, timeInSelector, timeExtendedInSelector)
import Data.Hourglass (DateTime(DateTime), Date (Date), Month (May), TimeOfDay (TimeOfDay), timeGetDate, timeGetTimeOfDay, getWeekDay)
import Parse.AST (OpeningHours(OpeningHours), RuleSequence (RuleSequence), RuleType (Normal), SelectorSequence (TimeSel), TimeSpan (Span))

main :: IO ()
main = print "Hello World"
