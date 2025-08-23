module Tok (Literal(..), Line, Token(..), TokenType(..), Pos(..), stringPos) where

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
    deriving Show

type Line = Int
data Pos = Pos { getLine :: Int,
                 getColumn :: Int }
                 deriving(Show, Eq)
data Token = Token { getType :: TokenType,
                     getPos :: Pos }
                     deriving(Show)

stringPos :: Pos -> String
stringPos (Pos x y) = "(Line " ++ (show x) ++ ", Col " ++ (show y)  ++ ")"