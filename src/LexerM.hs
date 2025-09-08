module LexerM (lexes, printTokens) where

{-
Currently, this lexes and reads in one pass. This leads to the awkward
situation where we need to use a function like reads or span and then
find its length in O(n) time to update our column counter.
In the future this will be replaced with custom functions that return
the characters consumed - then those strings will be read in another
pass.
-}

import Prelude hiding (lex)
import Check
import Data.Char(isSpace, isAlpha, isAlphaNum, isNumber)
import Data.List(stripPrefix)
import Tok(Literal(..), Token(..), TokenType(..), Pos(..), stringPos)
import Control.Monad.State
import Control.Monad.Error.Class (MonadError(throwError))
-- data LexerState = LexerState {
--     getCode :: String,
--     lexerPos :: Pos
-- }
type CheckStr = Check String
type LexerState = (String, Pos)
type Lexer a = StateT LexerState CheckStr a

symbols :: [String]
symbols = [".", ",", "++", "+", "-", "/*", "*/", "*", "//", "/", "(", ")", "{", "}", ";", "==", "=", "<=", ">=", "<", ">", "||", "&&", "!=",  "!"]
keywords :: [String]
keywords = ["fun", "var", "if", "else", "class", "return", "for", "break", "print", "super", "while", "this"]
literals :: [(String, Literal)]
literals = [("true", LBool True), 
            ("false", LBool False),
            ("nil", LNil)]

dictFind :: (Eq a) => a -> [(a, b)] -> Maybe b
dictFind _ [] = Nothing
dictFind key ((k, v):ds)
    | key == k = Just v
    | otherwise = dictFind key ds

lexes :: String -> Check String [Token]
lexes code = do
    (tokens, _) <- runStateT lex $ (code, Pos 1 1)
    return tokens

printTokens :: [Token] -> IO ()
printTokens [] = return ()
printTokens (t:ts) = do
    putStrLn $ (\(Token tt pos) -> (show tt) ++ stringPos pos) t
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


lexCommentLine :: Lexer Token
lexCommentLine = do
    _ <- lenSlice "//" "\n" False
    consume

lexCommentBlock :: Lexer Token
lexCommentBlock = do
    _ <- lenSlice "/*" "*/" True
    consume


lexWhitespace :: Lexer Token
lexWhitespace = do
    (_str, pos) <- get
    case _str of
        [] -> consume
        c:cs -> do
            put (cs, newlinePos pos c)
            consume

newlinePos :: Pos -> Char -> Pos
newlinePos (Pos ln _) '\n' = Pos (ln + 1) 1
newlinePos (Pos ln cl) _ = Pos ln (cl + 1)

lexAlpha :: Lexer Token
lexAlpha = do
    (_str, pos) <- get
    let (first, rest) = span (\x -> isAlphaNum x || x == '_') _str
        colOffset = length first
        newPos = addCols pos colOffset
    put (rest, newPos)
    let token = case dictFind first literals of
            Just t -> Lit t
            Nothing -> if first `elem` keywords
                then TokenKeyword first
                else TokenIdent first
    return $ Token token pos


lexDouble :: Lexer Token
lexDouble = do
    first <- lenSpan (\x -> isNumber x || x == '.') False
    (_, pos) <- get
    case (reads first :: [(Double, String)]) of
        (num, []):_ -> do
            let tt = Lit $ Number num
            return $ Token tt pos
        _ -> lift $ Error $ "Error lexing number: " ++ (show first) ++ stringPos pos

--needs to be updated so that it increments cols?
-- lenSpan :: (a -> Bool) -> [a] -> (Int, [a], [a])
-- lenSpan p zs = helper (0, zs, []) where
--     helper (count, [], xs) = (count, [], reverse xs)
--     helper (count, xs@(x:xs'), ys)
--         | p x = helper (count + 1, xs', x:ys)
--         | otherwise = (count, xs, reverse ys)

lenSpan :: (Char -> Bool) -> Bool -> Lexer String
lenSpan p strict = do
    (code, pos) <- get
    helper pos [] code where
        helper :: Pos -> String -> String -> Lexer String
        helper _pos xs []
            | strict = throwError $ "Expected closing condition to be met " ++ stringPos _pos
            | otherwise = do
                put ("", _pos)
                return (reverse xs)
        helper _pos xs (y:ys)
            | p y = helper (newlinePos _pos y) (y:xs) ys
            | y == '\n' = throwError $ "Unexpected end of line " ++ stringPos _pos
            | otherwise = do
                put (y:ys, _pos)
                return (reverse xs)

lenSlice :: [Char] -> [Char] -> Bool -> Lexer [Char]
lenSlice key term strict = do
    code <- checkPrefix key
    (_, p) <- get
    helper p [] code where
        helper :: Pos -> [Char] -> [Char] -> Lexer [Char]
        helper _pos xs []
            | strict = throwError $ "Expected terminator \"" ++ (show term) ++ "\"" ++ stringPos _pos
            | otherwise = do
                put ("", _pos)
                return (reverse xs)
        helper _pos xs (y:ys) = case stripPrefix term (y:ys) of
            Nothing -> helper (newlinePos _pos y) (y:xs) ys
            Just rest -> do
                put (rest, account _pos term)
                return (reverse xs)

account :: Pos -> String -> Pos
account = foldl newlinePos


lexSymbol :: Lexer Token
lexSymbol = symHelper symbols where
    symHelper :: [String] -> Lexer Token
    symHelper [] = do 
        (_, pos) <- get
        lift $ Error $ "Unrecognized symbol at " ++ stringPos pos
    symHelper (s:ss) = do
        (_str, pos) <- get
        case (stripPrefix s _str) of
            Nothing -> symHelper ss
            Just rest
                | s == "//" -> lexCommentLine
                | s == "/*" -> lexCommentBlock
                | s == "*/" -> lift $ Error ("Unexpected end of comment block " ++ stringPos pos)
                | otherwise -> do
                    let newPos = account pos s
                    put (rest, newPos)
                    return (Token (Symbol s) newPos)
-- lexString :: Lexer Token
-- lexString = do
--     (_str, pos) <- get
--     stripped <- checkPrefix "\""
--     let (first, rest) = span (/= '\"') stripped
--     case rest of
--         '\"':cs -> do
--             let colOffset = 1 + (length first) --length of quotes + string
--                 newPos = addCols pos colOffset
--             put (cs, newPos) 
--             let tt = Lit $ Str first
--             return $ Token tt pos
--         _ -> do
--             lift (Error ("Expected '\"' " ++ stringPos pos))

lexString :: Lexer Token
lexString = do
    (_, pos) <- get
    _ <- checkPrefix "\""
    text <- lenSpan (/='\"') True
    _ <- checkPrefix "\""
    return $ Token (Lit $ Str text) pos


checkPrefix :: String -> Lexer String
checkPrefix pre = do
    (str, pos) <- get
    case (stripPrefix pre str) of
        Nothing -> lift $ Error $ "Expected '" ++ (show pre) ++ "' " ++ stringPos pos 
        Just stripped -> do
            put (stripped, account pos pre)
            return stripped

addCols :: Pos -> Int -> Pos
addCols (Pos x y) offset = Pos x (y + offset)