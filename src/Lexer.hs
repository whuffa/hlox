module Lexer (lexes) where

import Prelude hiding (lex)
import Data.Char
import Data.List
import Tok (Literal(..), Token(..)) 

lexes :: String -> [Token]
lexes = lexer symbols keywords where
    symbols = [".", ",", "+", "-", "*", "/", "(", ")", "{", "}", ";", "==", "=", "<=", ">=", "<", ">", "||", "&&", "!=",  "!"]
    keywords = ["fun", "var", "if", "else", "true", "false", "class", "nil", "return", "for", "print", "super", "while", "this"]


lexer :: [String] -> [String] -> String -> [Token]
lexer symbols keywords str = lex str where
    lex [] = []
    lex (c:cs)
      | isSpace c = lex cs
      | isAlpha c = lexAlpha keywords (c:cs)
      | isDigit c = lexDigits (c:cs)
      | c == '"' = lexString cs
      | otherwise = lexSym symbols (c:cs)

    lexString :: String -> [Token]
    lexString _str = Lit (Str first) : lex rest where
        (first, _:rest) = span (/= '\"') _str

    lexSym :: [String] -> String -> [Token]
    lexSym (s:ss) cs =
        case stripPrefix s cs of
            Nothing -> lexSym ss cs
            Just rest -> Symbol s : lex rest
    lexSym [] (c:_) = error ("Unrecognized symbol '" ++ [c] ++ "'")
    lexSym [] [] = error ("Lmao wtf")

    lexDigits cs = Lit (Number num) : lex rest
        where [(num, rest)] = reads cs :: [(Double, String)]
    lexAlpha ks _str = token : lex rest where
        (first, rest) = span isAlphaNum _str
        token = if first `elem` ks
                then TokenKeyword first
                else TokenIdent first
