module Evaluate(evaluate, Value(..)) where

import Operators
import Tok (Literal(..))
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
truthiness _ = True

truth :: Expr -> Bool
truth = truthiness . evaluate

stringifyVal :: Value -> String
stringifyVal (NumVal num) = show num
stringifyVal (BoolVal bool) = show bool
stringifyVal (StringVal str) = str
stringifyVal Nil = "Nil"

evaluate :: Expr -> Value
evaluate (Litr lit) = case lit of
    (Number num) -> (NumVal num)
    (Str str) -> (StringVal str)
    (LBool bool) -> (BoolVal bool)
evaluate (Binary Or op1 op2) = BoolVal $ if truth op1
    then True
    else truth op2
evaluate (Binary And op1 op2) = BoolVal $ if not (truth op1)
    then False
    else truth op2
evaluate (Binary operator op1 op2) =
    case (evaluate op1, evaluate op2) of
        (NumVal x, NumVal y) -> NumVal $ case operator of
            Add -> x + y
            Sub -> x - y
            Mul -> x * y
            Div -> x / y
            op -> error $ "operator: " ++ (show op) ++ "not defined for numbers"
        (StringVal x, y) -> case operator of
            Add -> StringVal $ x ++ stringifyVal y
            _ -> error "piss"
        (x, StringVal y) -> case operator of
            Add -> StringVal $ stringifyVal x ++ y
            _ -> error "piss again"
        _ -> Nil
    
evaluate (Unary Not op) = BoolVal $ not $ truth op
evaluate (Unary operation op) =
    case (evaluate op) of
        (NumVal num) -> case operation of
            Neg -> NumVal (-num)
        _ -> Nil

evaluate (Group expr) = evaluate(expr)
