{-# LANGUAGE InstanceSigs #-}
module Tok (Literal(..), Token(..), TokenType(..), Pos(..), stringPos) where

data Literal = Number Double
             | Str String
             | LBool Bool
             | LNil
    deriving (Show, Eq)

data TokenType 
    = Lit Literal
    | Symbol String 
    | TokenKeyword String 
    | TokenIdent String
    | EOF
    deriving (Eq)

instance Show TokenType where
    show :: TokenType -> String
    show (Lit literal) = show literal
    show (Symbol str) = "Symbol: " ++ str
    show (TokenKeyword str) = "Keyword: " ++ str
    show (TokenIdent str ) = "Identifier: " ++ str
    show EOF = "EOF"

data Pos = Pos { getLine :: Int,
                 getColumn :: Int }
                 deriving Eq

instance Show Pos where
    show :: Pos -> String
    show (Pos x y) = "(Line " ++ show x ++ ", Column " ++ show y ++ ")"

instance Semigroup Pos where
    (<>) :: Pos -> Pos -> Pos
    (Pos x1 y1) <> (Pos x2 y2) = Pos (x1 + x2) (y1 + y2)
instance Monoid Pos where
    mempty :: Pos
    mempty = Pos 0 0
data Token = Token { getType :: TokenType,
                     getPos :: Pos }
                     deriving(Eq)

instance Show Token where
    show :: Token -> String
    show (Token tt pos) = "Token (" ++ (show tt) ++ ")" ++ (show pos)

stringPos :: Pos -> String
stringPos (Pos x y) = "(Line " ++ (show x) ++ ", Col " ++ (show y)  ++ ")"