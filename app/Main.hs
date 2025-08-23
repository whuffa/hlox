module Main (main) where

import qualified LexerM as M
import Operators(Expr)
import Check
--import PrintAst(printAst)
import Parsle(parsle)
import Evaluate(evaluate, Value(..))
import System.IO(hFlush, stdout)

main :: IO ()
main = lineLoop interpret

check :: String -> Check Expr
check code = do
    tokens <- M.lexes code
    node <- parsle tokens
    return node

interpret :: String -> IO ()
interpret code = putStrLn $ case check code of
    Checked v -> show $ evaluate v
    Error e -> e


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
lineLoop = inputLoop getLine

inputFile :: (String -> IO ()) -> IO ()
inputFile process = do
    contents <- getContents
    process contents

-- interpret :: String -> Value
-- interpret = evaluate . parsle . lexes