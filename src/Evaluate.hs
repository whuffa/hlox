module Evaluate(evaluate, Value(..)) where

import Operators
import Tok (Literal(..), stringPos)
import Prelude hiding(EQ, GT, LT)

data Value
    = BoolVal Bool
    | NumVal Double
    | StringVal String
    | Nil
    deriving (Show, Eq)

truthiness :: Value -> Bool
truthiness (NumVal 0) = False
truthiness (StringVal "") = False
truthiness (BoolVal bool) = bool
truthiness Nil = False
truthiness _ = True

truth :: Expr -> Bool
truth = truthiness . evaluate

stringifyVal :: Value -> String
stringifyVal (NumVal num) = show num
stringifyVal (BoolVal bool) = show bool
stringifyVal (StringVal str) = str
stringifyVal Nil = "Nil"

evaluate :: Expr -> Value
evaluate (Litr lit _) = case lit of
    (Number num) -> (NumVal num)
    (Str str) -> (StringVal str)
    (LBool bool) -> (BoolVal bool)
    LNil -> Nil
evaluate (Binary Or op1 op2 _) = BoolVal $ if truth op1
    then True
    else truth op2
evaluate (Binary And op1 op2 _) = BoolVal $ if not (truth op1)
    then False
    else truth op2
evaluate (Binary EQ op1 op2 _) = BoolVal $ case (evaluate op1, evaluate op2) of
    (NumVal x, NumVal y) -> x == y
    (StringVal x, StringVal y) -> x == y
    (BoolVal x, BoolVal y) -> x == y
    _ -> False
evaluate (Binary NE op1 op2 _) = BoolVal $ case (evaluate op1, evaluate op2) of
    (NumVal x, NumVal y) -> x /= y
    (StringVal x, StringVal y) -> x /= y
    (BoolVal x, BoolVal y) -> x /= y
    _ -> True
evaluate (Binary operator op1 op2 pos) =
    case (evaluate op1, evaluate op2) of
        (NumVal x, NumVal y) -> case operator of
            Add -> NumVal $ x + y
            Sub -> NumVal $ x - y
            Mul -> NumVal $ x * y
            Div -> NumVal $ x / y
            LT -> BoolVal $ x < y
            LE -> BoolVal $ x <= y
            GT -> BoolVal $ x > y
            GE -> BoolVal $ x >= y
            --op -> error $ "operator: " ++ (show op) ++ "not defined for numbers " ++ stringPos pos
        (StringVal x, y) -> case operator of
            Add -> StringVal $ x ++ stringifyVal y
            _ -> error "piss"
        (x, StringVal y) -> case operator of
            Add -> StringVal $ stringifyVal x ++ y
            _ -> error "piss again"
        _ -> Nil
    
evaluate (Unary Not op _) = BoolVal $ not $ truth op
evaluate (Unary operation op pos) =
    case (evaluate op) of
        (NumVal num) -> case operation of
            Neg -> NumVal (-num)
        _ -> Nil

evaluate (Group expr _) = evaluate(expr)
