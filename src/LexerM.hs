module LexerM (lexes) where

{-
Currently, this lexes and reads in one pass. This leads to the awkward
situation where we need to use a function like reads or span and then
find its length in O(n) time to update our column counter.
In the future this will be replaced with custom functions that return
the characters consumed - then those strings will be read in another
pass.
-}

import Prelude hiding (lex)
import Check(Check(..))
import Data.Char(isSpace, isAlpha, isAlphaNum, isNumber)
import Data.List(stripPrefix)
import Tok(Literal(..), Token(..), TokenType(..), Pos(..))
import Control.Monad.State
--import Control.Monad(liftM)

-- data LexerState = LexerState {
--     getCode :: String,
--     lexerPos :: Pos
-- }
type LexerState = (String, Pos)
type Lexer a = StateT LexerState Check a

symbols :: [String]
symbols = [".", ",", "++", "+", "-", "*", "/", "(", ")", "{", "}", ";", "==", "=", "<=", ">=", "<", ">", "||", "&&", "!=",  "!"]
keywords :: [String]
keywords = ["fun", "var", "if", "else", "true", "false", "class", "nil", "return", "for", "print", "super", "while", "this"]

lexes :: String -> IO ()
lexes code = case runStateT lex $ (code, Pos 1 1) of
    Checked (tokens, _) -> printTokens tokens
    Error string -> print(string)

printTokens :: [Token] -> IO ()
printTokens [] = return ()
printTokens (t:ts) = do
    print $ (\(Token tt pos) -> (show tt) ++ stringPos pos) t
    printTokens ts

lex :: Lexer [Token]
lex = do
    Token tokentype pos <- consume
    case tokentype of
        EOF -> return []
        other -> do
            (Token other pos) <: lex
            --liftM (:) return (Token other line) (lex)

(<:) :: (Monad m) => a -> m [a] -> m [a]
item <: monad = do
     a <- monad
     return (item:a)

consume :: Lexer Token
consume = do 
    (code, pos) <- get
    case code of
        [] -> return $ Token EOF pos
        (c:_)
            | isSpace c -> lexWhitespace
            | isAlpha c || c == '_' -> lexAlpha
            | isNumber c -> lexDouble
            | c == '"' -> lexString
            | otherwise -> lexSymbol

lexWhitespace :: Lexer Token
lexWhitespace = do
    (_str, pos) <- get
    case _str of
        [] -> consume
        c:cs -> do
            let Pos ln cl = pos
                (newLine, newCol) = if c == '\n'
                    then (ln + 1, 1)
                    else (ln, cl + 1)
                newPos = Pos newLine newCol
            put (cs, newPos)
            consume

lexAlpha :: Lexer Token
lexAlpha = do
    (_str, pos) <- get
    let (first, rest) = span (\x -> isAlphaNum x || x == '_') _str
        colOffset = length first
        newPos = addCols pos colOffset
    put (rest, newPos)
    let tok = if first `elem` keywords
        then TokenKeyword
        else TokenIdent
    return $ Token (tok first) pos


lexDouble :: Lexer Token
lexDouble = do
    (_str, pos) <- get
    let (colOffset, rest, first) = lenSpan (\x -> isNumber x || x == '.' ) _str
        newPos = addCols pos colOffset
    case (reads first :: [(Double, String)]) of
        (num, []):_ -> do
            put (rest, newPos)
            let tt = Lit $ Number num
            return $ Token tt pos
        _ -> lift $ Error $ "Error lexing number: " ++ (show first) ++ stringPos pos

lenSpan :: (a -> Bool) -> [a] -> (Int, [a], [a])
lenSpan p zs = helper (0, zs, []) where
    helper (count, [], xs) = (count, [], reverse xs)
    helper (count, xs@(x:xs'), ys)
        | p x = helper (count + 1, xs', x:ys)
        | otherwise = (count, xs, reverse ys)


lexSymbol :: Lexer Token
lexSymbol = do
    (_, _pos) <- get
    tt <- symHelper symbols
    return $ Token tt _pos
    where
        symHelper :: [String] -> Lexer TokenType
        symHelper [] = do 
            (_, pos) <- get
            lift $ Error $ "Unrecognized symbol at " ++ stringPos pos
        symHelper (s:ss) = do
            (_str, pos) <- get
            case (stripPrefix s _str) of
                Nothing -> symHelper ss
                Just rest -> do
                    let colOffset = length s
                        newPos = addCols pos colOffset
                    put (rest, newPos)
                    return $ Symbol s
lexString :: Lexer Token
lexString = do
    (_str, pos) <- get
    stripped <- checkPrefix "\""
    let (first, rest) = span (/= '\"') stripped
    case rest of
        '\"':cs -> do
            let colOffset = 2 + (length first) --length of quotes + string
                newPos = addCols pos colOffset
            put (cs, newPos) 
            let tt = Lit $ Str first
            return $ Token tt pos
        _ -> do
            lift (Error ("Expected '\"' " ++ stringPos pos))

checkPrefix :: String -> Lexer String
checkPrefix pre = do
    (str, pos) <- get
    case (stripPrefix pre str) of
        Nothing -> lift $ Error $ "Expected '" ++ (show pre) ++ "' " ++ stringPos pos 
        Just stripped -> do
            let colOffset = length pre
                newPos = addCols pos colOffset
            put (stripped, newPos)
            return stripped

-- addLines :: Pos -> Int -> Pos
-- addLines (Pos x y) offset = Pos (x + offset) y

addCols :: Pos -> Int -> Pos
addCols (Pos x y) offset = Pos x (y + offset)

stringPos :: Pos -> String
stringPos (Pos x y) = "(Line " ++ (show x) ++ ", Col " ++ (show y)  ++ ")"