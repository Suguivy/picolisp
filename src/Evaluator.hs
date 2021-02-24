module Evaluator where

import Expression
import Primitives

eval :: Expression -> Expression
eval n@(Number _) = n
eval s@(Symbol _) = s
eval (SExpr es)   = apply $ SExpr (map eval es)

apply :: Expression -> Expression
apply (SExpr (f:args)) = case f of
  Symbol "+" -> pAdd args
  Symbol "-" -> pSub args
  Symbol "*" -> pMul args
  Symbol "/" -> pDiv args
  Symbol _     -> error "no primitive functions not supported"
  _            -> error "object not applicable"
apply _                = error "Object not applicable"
