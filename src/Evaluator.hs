module Evaluator where

import Expression
import Primitives

-- Evals (reduces) an expression
eval :: Expression -> Either String Expression
eval n@(Number _) = Right n
eval s@(Symbol _) = Right s
eval (SExpr es)   = apply . SExpr =<< mapM eval es

-- Applies, in a SExpr, a function to its arguments
apply :: Expression -> Either String Expression
apply (SExpr (f:args)) = case f of
  Symbol "+" -> Right $ pAdd args
  Symbol "-" -> Right $ pSub args
  Symbol "*" -> Right $ pMul args
  Symbol "/" -> Right $ pDiv args
  Symbol x   -> Left  $ show x ++ ": no primitive functions not supported"
  _          -> Left  "object not applicable"
apply _                = Left "object not applicable"
