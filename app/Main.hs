module Main (main) where

import Lexer(lexes)
import Parsle(parsle)
import Evaluate(evaluate, Value(..))
import System.IO(hFlush, stdout)

main :: IO ()
main = do
    putStr "> "
    hFlush stdout
    input <- getLine
    if null input
        then return ()
        else do
            print (interpret input)
            main

interpret :: String -> Value
interpret = evaluate . parsle . lexes
