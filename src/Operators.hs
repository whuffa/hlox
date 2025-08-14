module Operators (BinaryOp(..), UnaryOp(..), Expr(..)) where

import Prelude hiding (EQ)
import Tok (Literal)

data BinaryOp = Add | Sub | Mul | Div |
                And | Or | GT | LT | LE |
                GE | EQ | NE
    deriving (Show, Eq)

data UnaryOp = Neg | Not
    deriving (Show, Eq)

data Expr
    = Litr Literal
    | Unary UnaryOp Expr
    | Binary BinaryOp Expr Expr
    | Group Expr
    deriving (Eq, Show)