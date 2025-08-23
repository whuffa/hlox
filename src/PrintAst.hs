module PrintAst(printAst) where

import Operators
import Prelude hiding(EQ, GT, LT)

printAst :: Expr -> IO ()
printAst node = do
    printNode node
    putChar '\n'

printNode :: Expr -> IO()
printNode (Litr lit _)  = putStr $  ' ':(show lit)
printNode (Unary op op1 _) = do
    putStr $ ' ':(show op)
    printNode op1
    return ()
printNode (Binary op op1 op2 _) = do
    printNode op1
    putStr $ ' ':(show op)
    printNode op2
    return ()
printNode (Group op1 _) = do
    putChar '('
    printNode op1
    putChar ')'
    return ()
