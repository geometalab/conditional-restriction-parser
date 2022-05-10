module Parse.AST where

type Token = String

type Grammar = [Expression]

type Expression = (Token, [Condition])

data Condition
  = OpeningHours OpeningHours
  | Comparison Token ComparisonOp Token

data ComparisonOp = Gt | Lt | GtEq | LtEq | Eq

data OpeningHours = OH_ -- TODO
