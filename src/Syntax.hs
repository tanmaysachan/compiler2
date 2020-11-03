module Syntax where

data Expr = Value Int
          | BinOp Op Expr Expr
          | Var String
          | Call String [Expr]
          | Function String [String] [Expr]
          | Return Expr
          deriving (Eq, Ord, Show)

data Op = Equals | Multiply | Divide | Plus | Minus deriving (Eq, Ord, Show)

class Branch a where
    isBranch :: a -> Bool

instance Branch Expr where
    isBranch (Function _ _ _) = True
    isBranch (Return _) = True
    isBranch (Call _ _) = True
    isBranch _ = False
