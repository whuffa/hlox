module Tok (Literal(..), Token(..)) where

data Literal = Number Double
             | Str String
    deriving (Show, Eq)

data Token = Lit Literal
           | Symbol String 
           | TokenKeyword String 
           | TokenIdent String
    deriving Show