module Main (main) where

import LexerM
import Operators
import Check
import Parsle(parsle)
import Interpreter
import System.IO
import Prelude hiding(lex)
import Tok(Token)
import Control.Monad.IO.Class(MonadIO(..))
import System.Environment(getArgs)
import Resolver


main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> run repl-- open repl
        [fname] -> do 
            handle <- openFile fname ReadMode -- open file and run through normally
            code <- hGetContents handle
            case lp code of
                Error msg -> putStrLn msg
                Checked pgrm -> do
                    rpgrm <- resolve pgrm
                    case rpgrm of
                        Error msg -> putStrLn msg
                        Checked rstmts -> run (interpret rstmts)
        _ -> putStrLn "Usage: Lox <filename>"


-- One do block carries the possibility of failure from lexes, parsing, and
-- interpreting. Then another function takes that and handles (prints) the error.



getCmnd :: IO String
getCmnd = do
    putStr "> "
    hFlush stdout
    getLine

repl :: Interpreter()
repl = do
    line <- liftIO getCmnd
    if line == "quit"
        then return()
        else do
            case lp line of
                Error msg -> liftIO (putStrLn msg)
                Checked pgrm -> interpret pgrm
            repl

lp :: String -> Check String [Stmt]
lp code = do
    tokens <- lex code
    parse tokens

lex :: String -> Check String [Token]
lex = lexes

parse :: [Token] -> Check String [Stmt]
parse = parsle

-- hello i am corbin