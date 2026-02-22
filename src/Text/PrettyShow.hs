module Text.PrettyShow (SymbolicExpr (Symbol), prettyShow, prettyShowI, prettyShowDL, simplify) where

import Data.DList (DList)
import qualified Data.DList as DList

data BinaryOp = Plus | Minus | Mul deriving (Eq, Show)

data UnaryOp = Abs | Negate | Signum deriving (Eq, Show)

data SymbolicExpr a
  = Number a
  | Symbol String
  | BinaryArith BinaryOp (SymbolicExpr a) (SymbolicExpr a)
  | UnaryArith UnaryOp (SymbolicExpr a)

instance (Num a) => Num (SymbolicExpr a) where
  fromInteger = Number . fromInteger
  (+) = BinaryArith Plus
  (-) = BinaryArith Minus
  (*) = BinaryArith Mul
  abs = UnaryArith Abs
  negate = UnaryArith Negate
  signum = UnaryArith Signum

prettyShowDLBinaryOp :: BinaryOp -> DList Char
prettyShowDLBinaryOp Plus = DList.singleton '+'
prettyShowDLBinaryOp Minus = DList.singleton '-'
prettyShowDLBinaryOp Mul = DList.singleton '*'

prettyShowDLUnaryOp :: UnaryOp -> DList Char
prettyShowDLUnaryOp Abs = DList.fromList "abs "
prettyShowDLUnaryOp Negate = DList.singleton '-'
prettyShowDLUnaryOp Signum = DList.fromList "signum "

-- | Show an expression with parentheses around it except integer literals.
prettyShowDLNestedExpr :: (Show a) => SymbolicExpr a -> DList Char
prettyShowDLNestedExpr x@(Number _) = prettyShowDL x
prettyShowDLNestedExpr x@(Symbol _) = prettyShowDL x
prettyShowDLNestedExpr x = mconcat [DList.singleton '(', prettyShowDL x, DList.singleton ')']

prettyShowDL :: (Show a) => SymbolicExpr a -> DList Char
prettyShowDL (Number x) = DList.fromList $ show x
prettyShowDL (Symbol x) = DList.fromList x
prettyShowDL (BinaryArith op x y) = mconcat [prettyShowDLNestedExpr x, prettyShowDLBinaryOp op, prettyShowDLNestedExpr y]
prettyShowDL (UnaryArith op x) = mconcat [prettyShowDLUnaryOp op, prettyShowDLNestedExpr x]

prettyShow :: (Show a) => SymbolicExpr a -> String
prettyShow = DList.toList . prettyShowDL

prettyShowI :: SymbolicExpr Integer -> String
prettyShowI = prettyShow

simplify :: (Num a, Eq a) => SymbolicExpr a -> SymbolicExpr a
simplify (BinaryArith op ix iy) = case (op, sx, sy) of
  (Mul, Number 1, y) -> y
  (Mul, x, Number 1) -> x
  (Plus, Number 0, y) -> y
  (Plus, x, Number 0) -> x
  _ -> (BinaryArith op sx sy)
  where
    sx = simplify ix
    sy = simplify iy
simplify (UnaryArith op ix) = UnaryArith op (simplify ix)
simplify expr = expr
