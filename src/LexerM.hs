module LexerM (lexes) where

import Prelude hiding (lex)
import Check(Check(..))
import Data.Char(isSpace, isAlpha, isAlphaNum, isSymbol, isNumber)
--import Data.List(stripPrefix)
import Tok(Literal(..), Token(..), Line, TokenType(..))
import Control.Monad.State

type LexerState = (String, Line)
type Lexer a = StateT LexerState Check   a

symbols :: [String]
symbols = [".", ",", "+", "-", "*", "/", "(", ")", "{", "}", ";", "==", "=", "<=", ">=", "<", ">", "||", "&&", "!=",  "!"]
keywords :: [String]
keywords = ["fun", "var", "if", "else", "true", "false", "class", "nil", "return", "for", "print", "super", "while", "this"]

lexes :: String -> [Token]
lexes _ = [(Token EOF 9001)]

consume :: Lexer TokenType
consume = do 
    (code, line) <- get
    case code of
        [] -> return EOF
        (c:cs)
            | c == '\n' -> do 
                put (cs, line + 1)
                consume
            | isSpace c -> do
                put (cs, line)
                consume
            | isAlpha c || c == '_' -> let (token, remaining) = lexAlpha (c:cs) in do
                put (remaining, line)
                return (token)
            | isNumber c -> let (token, remaining) = lexDouble (c:cs) in do
                put (remaining, line)
                return (token)
            | c == '"' -> let (token, remaining) = lexString cs in do
                put (remaining, line)
                return (token)
            | otherwise -> let (token, remaining) = lexSymbol (c:cs) in do  
                put (remaining, line)
                return (token)

lexAlpha :: String -> (TokenType, String)
lexAlpha cs = (token, remaining) where
    (first, remaining) = span (\x -> isAlphaNum x || x == '_') cs
    token = if first `elem` keywords
        then TokenKeyword first
        else TokenIdent first

lexDouble :: String -> (TokenType, String)
lexDouble cs = (Lit (Number num), remaining) where
    [(num, remaining)] = reads cs :: [(Double, String)]

lexSymbol :: String -> (TokenType, String)
lexSymbol cs = (Symbol sym, remaining) where
    (sym, remaining) = span isSymbol cs

lexString :: String -> (TokenType, String)
lexString _str = (Lit (Str first), rest) where
    (first, _:rest) = span (/= '\"') _str
