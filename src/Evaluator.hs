module Evaluator where

import Expression
import Primitives

eval :: Expression -> Either String Expression
eval n@(Number _) = Right n
eval s@(Symbol _) = Right s
eval (SExpr es)   = mapM eval es >>= apply . SExpr

apply :: Expression -> Either String Expression
apply (SExpr (f:args)) = case f of
  Symbol "+" -> Right $ pAdd args
  Symbol "-" -> Right $ pSub args
  Symbol "*" -> Right $ pMul args
  Symbol "/" -> Right $ pDiv args
  Symbol _     -> Left "no primitive functions not supported (only +, -, * and /)"
  _            -> Left "object not applicable"
apply _                = Left "object not applicable"
