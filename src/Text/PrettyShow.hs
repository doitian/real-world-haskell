module Text.PrettyShow (prettyShow) where

data PrettyExpr
  = PrettyFromInteger Integer
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
prettyShowInParens x = "(" ++ prettyShow x ++ ")"

prettyShow :: PrettyExpr -> String
prettyShow (PrettyFromInteger x) = show x
prettyShow (PrettyAddition x y) = prettyShowInParens x ++ "+" ++ prettyShowInParens y
prettyShow (PrettySubtraction x y) = prettyShowInParens x ++ "-" ++ prettyShowInParens y
prettyShow (PrettyMultiply x y) = prettyShowInParens x ++ "*" ++ prettyShowInParens y
prettyShow (PrettyAbs x) = "abs " ++ prettyShowInParens x
prettyShow (PrettyNegate x) = "-" ++ prettyShowInParens x
prettyShow (PrettySignum x) = "signum " ++ prettyShowInParens x
