module Primitives where

import Expression

pAdd, pSub, pMul, pDiv :: [Expression] -> Expression

pAdd = foldr1 $ \(Number a) (Number b) -> Number (a+b)
pSub = foldr1 $ \(Number a) (Number b) -> Number (a-b)
pMul = foldr1 $ \(Number a) (Number b) -> Number (a*b)
pDiv = foldr1 $ \(Number a) (Number b) -> Number (a/b)
