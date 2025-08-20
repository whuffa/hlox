module Main (main) where

import Lexer(lexes)
import qualified LexerM as M
import Parsle(parsle)
import Evaluate(evaluate, Value(..))
import System.IO(hFlush, stdout)

main :: IO ()
main = inputFile M.lexes

-- hello i am corbin
inputLoop :: (IO String) -> (String -> IO ()) -> IO ()
inputLoop content process = do
    putStr "> "
    hFlush stdout
    input <- content
    if null input
        then return ()
        else do
            process input
            inputLoop content process

lineLoop :: (String -> IO ()) -> IO ()
lineLoop process = inputLoop getLine process

inputFile :: (String -> IO ()) -> IO ()
inputFile process = do
    contents <- getContents
    process contents

interpret :: String -> Value
interpret = evaluate . parsle . lexes