{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Lexer(lexes)
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
import Control.Monad.State(StateT(..), runStateT)
import Control.Monad.State(MonadState(put, get))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

data ReplState = ReplState { interpreterState :: InterpreterState,
                             resolverState :: ResolverState }

type Repl a = StateT ReplState IO a

repl' :: Repl ()
repl' = do
    line <- liftIO getCmnd
    if line == "quit"
        then return ()
        else do
            case lp line of
                Error msg -> liftIO (putStrLn msg)
                Checked pgrm -> do
                    _state <- get
                    (rpgrm, rState) <- liftIO $ resolve' pgrm (resolverState _state)
                    case rpgrm of
                        Error msg -> liftIO (putStrLn msg)
                        Checked rstmts -> do
                            (res, iState) <- liftIO $ interpret' rstmts (interpreterState _state)
                            case res of
                                Error (RuntimeError msg) -> liftIO (putStrLn msg)
                                Error x -> liftIO $ putStrLn $ "Failed to semantic analysis somewhere - " ++ show x ++ " slipped through."
                                Checked _ -> do
                                    put (ReplState iState rState)
            repl'


main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> do
            _ <- (runStateT repl') (ReplState initInterpreter initResolver)
            return ()
        [fname] -> do 
            handle <- openFile fname ReadMode -- open file and run through normally
            code <- TIO.hGetContents handle
            case lp code of
                Error msg -> putStrLn msg
                Checked pgrm -> do
                    rpgrm <- resolve pgrm
                    case rpgrm of
                        Error msg -> putStrLn msg
                        Checked rstmts -> interpret rstmts
        _ -> putStrLn "Usage: Lox <filename>"

getCmnd :: IO T.Text
getCmnd = do
    putStr "> "
    hFlush stdout
    TIO.getLine


lp :: T.Text -> Check String [Stmt]
lp code = do
    tokens <- lex code
    parse tokens

lex :: T.Text -> Check String [Token]
lex = lexes

parse :: [Token] -> Check String [Stmt]
parse = parsle

-- hello i am corbin