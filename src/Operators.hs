module Operators (BinaryOp(..), UnaryOp(..), Expr(..), Stmt(..), Prgm, Ident(..)) where

import Prelude hiding (EQ)
import Tok


data Ident = Ident { getName :: String,
                     getDepth :: Int,
                     getIPos :: Pos }
                     deriving (Show, Eq)
                     
data BinaryOp = Add | Sub | Mul | Div |
                And | Or | GT | LT | LE |
                GE | EQ | NE
    deriving (Show, Eq)

data UnaryOp = Neg | Not
    deriving (Show, Eq)



data Stmt
    = Print Expr Pos
    | StmtExpr Expr
    | Declaration Ident Expr Pos
    | Block [Stmt] Pos
    | IfElse Expr Stmt (Maybe Stmt) Pos
    | While Expr Stmt Pos
    | Break Pos
    | Return Expr Pos
    deriving (Eq, Show)

data Expr
    = Litr Literal Pos
    | Unary UnaryOp Expr Pos
    | Binary BinaryOp Expr Expr Pos
    | Assign Ident Expr Pos
    | Group Expr Pos
    | Identifier Ident
    | Call Expr [Expr] Pos
    | Lambda [Ident] [Ident] [Stmt] Pos
    deriving (Eq, Show)



type Prgm = [Stmt]