{-# LANGUAGE OverloadedStrings #-}
module Lexer (lexes) where



import Prelude hiding (lex)
import Check
import Data.Char(isSpace, isAlpha, isAlphaNum, isNumber)
import Data.List(foldl')
import Tok(Literal(..), Token(..), TokenType(..), Pos(..))
import Control.Monad.State
import Control.Monad.Error.Class (MonadError(throwError))
import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as B
import Data.Text.Lazy (toStrict)

data LexerState = LexerState { getRemains :: !T.Text,
                               getLPos :: !Pos }
type Lexer a = StateT LexerState (Check String) a


getCode :: Lexer T.Text
getCode = do
    _st <- get
    return (getRemains _st)

putCode :: T.Text -> Lexer ()
putCode remains = do
    _st <- get
    put (_st {getRemains = remains})

getStatePos :: Lexer Pos
getStatePos = do
    _st <- get
    return (getLPos _st)

putStatePos :: Pos -> Lexer ()
putStatePos pos = do
    _st <- get
    put (_st {getLPos = pos})

symbols :: [T.Text]
symbols = [".", ",", "+", "-", "/*", "*/", "*", "//", "/", "(", ")", "{", "}", ";", "==", "=", "<=", ">=", "<", ">", "||", "&&", "!=",  "!"]
keywords :: [T.Text]
keywords = ["fun", "var", "if", "else", "class", "return", "for", "break", "print", "super", "while", "this"]
literals :: [(T.Text, Literal)]
literals = [("true", LBool True), 
            ("false", LBool False),
            ("nil", LNil)]

escape :: [(Char, Char)]
escape = [('\\', '\\'), ('\"', '\"'), ('n', '\n')]

dictFind :: (Eq a) => a -> [(a, b)] -> Maybe b
dictFind _ [] = Nothing
dictFind key ((k, v):ds)
    | key == k = Just v
    | otherwise = dictFind key ds

lexes :: T.Text -> Check String [Token]
lexes code = do
    (tokens, _) <- runStateT lexer $ LexerState code (Pos 1 1)
    return tokens

lexer :: Lexer [Token]
lexer = do
    tuples <- scans
    mapM lex tuples


scans :: Lexer [(T.Text, Pos)]
scans = do
    t@(text, _) <- scan
    case text of
        "EOF" -> return []
        _ -> do
            rest <- scans
            return (t:rest)

lex :: (T.Text, Pos) -> Lexer Token
lex (text, pos) = do
    tt <- case T.uncons text of
        Just (c, rest)
            | c == '\"' -> return $ Lit (Str rest)
            | isAlpha c || c == '_' -> return $ if text `elem` keywords
                then (TokenKeyword text)
                else case dictFind text literals of 
                    Nothing -> (TokenIdent text)
                    Just lit -> (Lit lit)
            | isNumber c -> case (reads (T.unpack text) :: [(Double, String)]) of
                (num, []):_ -> return $ Lit (Number num)
                _ -> throwError $ "Error lexing number \"" ++ T.unpack text ++ "\" " ++ show pos
            | otherwise -> return $ Symbol text
        Nothing -> throwError $ "Something went wrong lexing - empty string. " ++ show pos
    return (Token tt pos)



scan :: Lexer (T.Text, Pos)
scan = do
    code <- getCode
    case T.uncons code of
        Just (c, _)
            | isSpace c -> scanWhitespace
            | isAlpha c || c == '_' -> scanAlpha
            | isNumber c -> scanDouble
            | c == '"' -> scanString
            | otherwise -> scanSymbol
        Nothing -> do
            pos <- getStatePos
            return ("EOF", pos)

scanWhitespace :: Lexer (T.Text, Pos)
scanWhitespace = do
    _ <- span' isSpace
    scan

scanAlpha :: Lexer (T.Text, Pos)
scanAlpha = span' (\x -> isAlphaNum x || x == '_')

scanDouble :: Lexer (T.Text, Pos)
scanDouble = span' (\x -> isNumber x || x == '.')

scanCommentLine :: Lexer (T.Text, Pos)
scanCommentLine = do
    _ <- slice (T.singleton '\n') False
    scan

scanCommentBlock :: Lexer (T.Text, Pos)
scanCommentBlock = do
    _ <- slice "*/" True
    scan

scanString :: Lexer (T.Text, Pos)
scanString = do
    code <- getCode
    pos <- getStatePos
    case T.uncons code of
        Just ('\"', rest) -> do
            (builder, rest', pos') <- helper (B.fromString "") rest pos
            putCode rest'
            putStatePos pos'
            return ((toStrict . B.toLazyText) (B.singleton '\"' <> builder), newlinePos pos' '\"')
        _ -> throwError $ "Something went wrong scanning - expected opening quotation mark. " ++ show pos
    where
        helper :: B.Builder -> T.Text -> Pos -> Lexer (B.Builder, T.Text, Pos)
        helper builder text pos = do
            (c, rest, pos') <- scanHelper text pos
            case c of
                '\\' -> do
                    (c', rest', pos'') <- scanHelper rest pos'
                    case dictFind c' escape of
                        Just esc -> helper (builder <> B.singleton esc) rest' (newlinePos pos'' c')
                        Nothing -> throwError $ "Unexpected escape character \"\\" ++ [c'] ++ "\" " ++ show pos'
                '\"' -> return (builder, rest, pos')
                _
                    | c `elem` ['\n'] -> throwError $ "Expected closing quotation mark. " ++ show pos
                    | otherwise -> helper (builder <> B.singleton c) rest pos'

scanHelper :: T.Text -> Pos -> Lexer (Char, T.Text, Pos)
scanHelper text pos = case T.uncons text of
    Nothing -> throwError $ "Expected closing quotation mark. " ++ show pos
    Just (c, rest) -> return (c, rest, newlinePos pos c)

scanSymbol :: Lexer (T.Text, Pos)
scanSymbol = do
    pos <- getStatePos
    code <- getCode
    helper pos code where
        helper pos code = helper' symbols where
            helper' (symbol:ss) = case T.stripPrefix symbol code of
                Just rest -> do
                    putCode rest
                    putStatePos (account pos symbol)
                    case symbol of
                        "//" -> scanCommentLine
                        "/*" -> scanCommentBlock
                        "*/" -> throwError $ "Unexpected end of comment block. " ++ (show pos)
                        _ -> do
                            putCode rest
                            putStatePos (account pos symbol)
                            return (symbol, pos)
                Nothing -> helper' ss
            helper' [] = throwError $ "Unrecognized symbol. " ++ show pos


span' :: (Char -> Bool) -> Lexer (T.Text, Pos)
span' p = do
    code <- getCode
    pos <- getStatePos
    let (first, rest) = T.span p code
    putCode rest
    putStatePos (account pos first)
    return (first, pos)

slice :: T.Text -> Bool -> Lexer (T.Text, Pos)
slice term strict = do
    code <- getCode
    pos <- getStatePos
    case (T.breakOn term code, strict) of
        ((_, ""), True) -> throwError $ "Expected closing condition to be met " ++ show pos
        ((first, rest), _) -> do
            let rest' = T.drop 2 rest
                pos' = account pos first
            putCode rest'
            putStatePos (account pos' term)
            return (first, pos)

-- checkPrefix :: T.Text -> Lexer T.Text
-- checkPrefix pre = do
--     code <- getCode
--     pos <- getStatePos
--     case (T.stripPrefix pre code) of
--         Nothing -> throwError $ "Expected '" ++ show pre ++ "' " ++ show pos 
--         Just stripped -> do
--             putCode stripped
--             putStatePos (account pos pre)
--             return stripped


account :: Pos -> T.Text -> Pos
account = T.foldl' newlinePos

-- account' :: Pos -> String -> Pos
-- account' = foldl' newlinePos

newlinePos :: Pos -> Char -> Pos
newlinePos (Pos ln _) '\n' = Pos (ln + 1) 1
newlinePos (Pos ln cl) _ = Pos ln (cl + 1)