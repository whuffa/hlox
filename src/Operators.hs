{-# LANGUAGE InstanceSigs #-}
module Operators (BinaryOp(..), UnaryOp(..), Expr(..), Stmt(..), Prgm, Ident(..)) where

import Prelude hiding (EQ)
import Tok
import qualified Data.Text as T


data Ident = Ident { getName :: !T.Text,
                     getDepth :: !Int,
                     getIPos :: Pos }
                     deriving (Show)


instance Eq Ident where
    (==) :: Ident -> Ident -> Bool
    (==) (Ident n1 _ _) (Ident n2 _ _) = (==) n1 n2

instance Ord Ident where
    compare :: Ident -> Ident -> Ordering
    (Ident n1 _ _) `compare` (Ident n2 _ _) = compare n1 n2
    (<=) :: Ident -> Ident -> Bool
    (<=) (Ident n1 _ _) (Ident n2 _ _) = n1 <= n2
                     
data BinaryOp = Add | Sub | Mul | Div |
                And | Or | GT | LT | LE |
                GE | EQ | NE
    deriving (Show, Eq)

data UnaryOp = Neg | Not
    deriving (Show, Eq)



data Stmt
    = Print Expr Pos
    | StmtExpr Expr
    | Declaration !Ident Expr Pos
    | Block [Stmt] Pos
    | IfElse Expr Stmt (Maybe Stmt) Pos
    | While Expr Stmt Pos
    | Break Pos
    | Return Expr Pos
    deriving (Eq, Show)

data Expr
    = Litr !Literal Pos
    | Unary !UnaryOp Expr Pos
    | Binary !BinaryOp Expr Expr Pos
    | Assign !Ident Expr Pos
    | Group Expr Pos
    | Identifier !Ident
    | Call Expr [Expr] Pos
    | Lambda [Ident] [Ident] [Stmt] Pos
    deriving (Eq, Show)



type Prgm = [Stmt]