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
    deriving (Eq, Show)

data Pos = Pos { getLine :: Int,
                 getColumn :: Int }
                 deriving(Show, Eq)

instance Semigroup Pos where
    (<>) :: Pos -> Pos -> Pos
    (Pos x1 y1) <> (Pos x2 y2) = Pos (x1 + x2) (y1 + y2)
instance Monoid Pos where
    mempty :: Pos
    mempty = Pos 0 0
data Token = Token { getType :: TokenType,
                     getPos :: Pos }
                     deriving(Show, Eq)

stringPos :: Pos -> String
stringPos (Pos x y) = "(Line " ++ (show x) ++ ", Col " ++ (show y)  ++ ")"