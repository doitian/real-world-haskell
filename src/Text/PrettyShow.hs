module Text.PrettyShow (PrettyExpr (Symbol), prettyShow, simplify) where

data PrettyExpr
  = PrettyFromInteger Integer
  | Symbol String
  | PrettyAddition PrettyExpr PrettyExpr
  | PrettySubtraction PrettyExpr PrettyExpr
  | PrettyMultiply PrettyExpr PrettyExpr
  | PrettyAbs PrettyExpr
  | PrettyNegate PrettyExpr
  | PrettySignum PrettyExpr

instance Num PrettyExpr where
  fromInteger = PrettyFromInteger
  (+) = PrettyAddition
  (-) = PrettySubtraction
  (*) = PrettyMultiply
  abs = PrettyAbs
  negate = PrettyNegate
  signum = PrettySignum

-- | Show an expression with parentheses around it except integer literals.
prettyShowInParens :: PrettyExpr -> String
prettyShowInParens (PrettyFromInteger x) = show x
prettyShowInParens (Symbol x) = x
prettyShowInParens x = "(" ++ prettyShow x ++ ")"

prettyShow :: PrettyExpr -> String
prettyShow (PrettyFromInteger x) = show x
prettyShow (Symbol x) = x
prettyShow (PrettyAddition x y) = prettyShowInParens x ++ "+" ++ prettyShowInParens y
prettyShow (PrettySubtraction x y) = prettyShowInParens x ++ "-" ++ prettyShowInParens y
prettyShow (PrettyMultiply x y) = prettyShowInParens x ++ "*" ++ prettyShowInParens y
prettyShow (PrettyAbs x) = "abs " ++ prettyShowInParens x
prettyShow (PrettyNegate x) = "-" ++ prettyShowInParens x
prettyShow (PrettySignum x) = "signum " ++ prettyShowInParens x

simplify :: PrettyExpr -> PrettyExpr
simplify (PrettyAddition (PrettyFromInteger 0) x) = x
simplify (PrettyAddition x (PrettyFromInteger 0)) = x
simplify (PrettySubtraction x (PrettyFromInteger 0)) = x
simplify (PrettySubtraction (PrettyFromInteger 0) x) = PrettyNegate x
simplify (PrettyMultiply (PrettyFromInteger 1) x) = x
simplify (PrettyMultiply x (PrettyFromInteger 1)) = x
simplify (PrettyAbs x@(PrettyAbs _)) = x
simplify (PrettyAbs (PrettyNegate x)) = (PrettyAbs x)
simplify (PrettyNegate (PrettyNegate x)) = x
simplify expr = expr
