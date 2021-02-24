module Expression where

-- The basic unit of the language are Expressions, so we make an
-- Expression data with the primitive expression values as the
-- constructors
data Expression = Number Double
                | Symbol String
                | SExpr [Expression]
                deriving (Eq)

instance Show Expression where
  show (Number n) = show n
  show (Symbol s) = show s
  show (SExpr es) = "(" ++ unwords (map show es) ++ ")"
