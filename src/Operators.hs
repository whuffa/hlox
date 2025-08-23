module Operators (BinaryOp(..), UnaryOp(..), Expr(..)) where

import Prelude hiding (EQ)
import Tok (Literal, Pos)

data BinaryOp = Add | Sub | Mul | Div |
                And | Or | GT | LT | LE |
                GE | EQ | NE
    deriving (Show, Eq)

data UnaryOp = Neg | Not
    deriving (Show, Eq)

data Expr
    = Litr Literal Pos
    | Unary UnaryOp Expr Pos
    | Binary BinaryOp Expr Expr Pos
    | Group Expr Pos
    deriving (Eq, Show)