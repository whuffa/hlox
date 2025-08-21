module Tok (Literal(..), Line, Token(..), TokenType(..), Pos(..)) where

data Literal = Number Double
             | Str String
             | LBool Bool
    deriving (Show, Eq)

data TokenType 
    = Lit Literal
    | Symbol String 
    | TokenKeyword String 
    | TokenIdent String
    | EOF
    deriving Show

type Line = Int
data Pos = Pos { getLine :: Int,
                 getColumn :: Int }
                 deriving(Show, Eq)
data Token = Token { getType :: TokenType,
                     getPos :: Pos }
                     deriving(Show)