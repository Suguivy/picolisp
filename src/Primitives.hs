module Primitives (pAdd, pSub, pMul, pDiv) where

-- This module exports the primitve expressions for our language

import Expression

pArith :: (Double -> Double -> Double) -> [Expression] -> Expression
pArith f es = Number . foldr1 f $ map unnum es
  where unnum = \(Number x) -> x

pAdd, pSub, pMul, pDiv :: [Expression] -> Expression
pAdd = pArith (+)
pSub = pArith (-)
pMul = pArith (*)
pDiv = pArith (/)
