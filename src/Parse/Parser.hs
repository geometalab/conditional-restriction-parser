module Parse.Parser where
import Parse.AST (Expression (Expression), ConditionalRestriction (ConditionalRestriction), Condition (..), ComparisonOp (..))
import Parse.Lib (Parser, word, noneOf, tok, str, anyOf, dbl, ws, strip)
import Control.Applicative (optional, Alternative (many, (<|>)))
import Data.Functor ((<&>))
import Parse.OpeningHoursParser (pOpeningHours)

pConditionalRestriction :: Parser String ConditionalRestriction
pConditionalRestriction = pack <$> pExpression <*> optional (word ";" *> pExpression)
  where
    pack x (Just y) = ConditionalRestriction [x, y]
    pack x Nothing = ConditionalRestriction  [x]

pExpression :: Parser String Expression
pExpression = Expression <$> (strip <$> many (noneOf "@")) <*> (word "@" *> pMultipleConditions)

pMultipleConditions :: Parser String [Condition]
pMultipleConditions = (word "(" *> pMultipleConditions <* word ")")
                  <|> (:) <$> pCondition <*> many (word "AND" *> pCondition)

pCondition :: Parser String Condition
pCondition = (word "(" *> pCondition <* word ")")
         <|> (OH <$> pOpeningHours)
         <|> (Comparison <$> pIdentifier <*> pCompOperator <*> dbl <* ws)
         <|> (Absolute <$> pIdentifier)

pCompOperator :: Parser String ComparisonOp
pCompOperator = (GtEq <$ word ">=")
            <|> (LtEq <$ word "<=")
            <|> (Eq <$ word "=")
            <|> (Lt <$ word "<")
            <|> (Gt <$ word ">")

pIdentifier :: Parser String String
pIdentifier = many (anyOf $ ['a'..'z'] ++ ['_', '-']) <* ws
