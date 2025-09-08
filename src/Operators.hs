module Operators (BinaryOp(..), UnaryOp(..), Expr(..), Stmt(..), Prgm) where

import Prelude hiding (EQ)
import Tok

type Ident = Token

data BinaryOp = Add | Sub | Mul | Div |
                And | Or | GT | LT | LE |
                GE | EQ | NE
    deriving (Show, Eq)

data UnaryOp = Neg | Not
    deriving (Show, Eq)



data Stmt
    = Print Expr Pos
    | StmtExpr Expr
    | Declaration Ident (Maybe Expr) Pos
    | Block [Stmt] Pos
    | IfElse Expr Stmt (Maybe Stmt) Pos
    | While Expr Stmt Pos
    | Break Pos

data Expr
    = Litr Literal Pos
    | Unary UnaryOp Expr Pos
    | Binary BinaryOp Expr Expr Pos
    | Assign Ident Expr Pos
    | Group Expr Pos
    | Identifier Ident
    deriving (Eq, Show)

type Prgm = [Stmt]