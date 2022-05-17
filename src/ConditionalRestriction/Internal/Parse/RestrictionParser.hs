-- | Parser for conditional restrictions.
module ConditionalRestriction.Internal.Parse.RestrictionParser where

import ConditionalRestriction.Internal.Parse.OpeningHoursParser
  ( pOpeningHours,
  )
import ConditionalRestriction.Internal.Parse.ParserLib
  ( Parser,
    anyOf,
    dbl,
    noneOf,
    strip,
    word,
    ws,
  )
import ConditionalRestriction.Parse.AST
  ( ComparisonOp (..),
    Condition (..),
    ConditionalRestriction (..),
    Expression (..),
  )
import Control.Applicative (Alternative (many, (<|>), some))
import Data.Functor ()

-- | Parse conditional restrictions, e.g. @"90 @ 18:00-22:00; 50 @ wet"@.
pConditionalRestriction :: Parser String ConditionalRestriction
pConditionalRestriction = ConditionalRestriction <$> ((:) <$> pExpression <*> many (word ";" *> pExpression))

pExpression :: Parser String Expression
pExpression = Expression <$> (strip <$> many (noneOf "@")) <*> (word "@" *> pMultipleConditions)

pMultipleConditions :: Parser String [Condition]
pMultipleConditions =
  (word "(" *> pMultipleConditions <* word ")")
    <|> (:) <$> pCondition <*> many (word "AND" *> pCondition)

pCondition :: Parser String Condition
pCondition =
  (word "(" *> pCondition <* word ")")
    <|> (OH <$> pOpeningHours)
    <|> (Comparison <$> pIdentifier <*> pCompOperator <*> dbl <* ws)
    <|> (Absolute <$> pIdentifier)

pCompOperator :: Parser String ComparisonOp
pCompOperator =
  (GtEq <$ word ">=")
    <|> (LtEq <$ word "<=")
    <|> (Eq <$ word "=")
    <|> (Lt <$ word "<")
    <|> (Gt <$ word ">")

pIdentifier :: Parser String String
pIdentifier = some (anyOf $ ['a' .. 'z'] ++ ['_', '-']) <* ws
