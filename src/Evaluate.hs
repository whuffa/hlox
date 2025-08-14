module Evaluate(evaluate, Value(..)) where

import Operators
import Tok (Literal(..))

data Value
    = BoolVal Bool
    | NumVal Double
    | StringVal String
    | Nil
    deriving (Show, Eq)



evaluate :: Expr -> Value
evaluate (Litr lit) = case lit of
    (Number num) -> (NumVal num)
    (Str str) -> (StringVal str)
evaluate (Binary operation op1 op2) = 
    case (left, right) of
        (NumVal num1, NumVal num2) -> NumVal $ numOp operation num1 num2 where
            numOp op x y = case op of 
                Add -> x + y
                Sub -> x - y
                Mul -> x * y
                Div -> x / y
                _ -> error("Nope!")
        (StringVal str1, StringVal str2) -> StringVal $ strOp operation str1 str2 where
            strOp op s t = case op of
                Add -> s ++ t
                _ -> error("Illegal use of operator on strings.")

        (BoolVal bool1, BoolVal bool2) -> BoolVal $ boolOp operation bool1 bool2 where 
            boolOp op x y = case op of
                Or -> error("Not yet implemented!")
                And -> error("Not yet implemented!")
                _ -> error("Unrecognized operators!")
        (_, _) -> Nil 
    where
        left = evaluate(op1)
        right = evaluate(op2)
evaluate (Unary operation op) =
    case val of
        (NumVal num) -> NumVal (numOp operation num) where
            numOp operator x = case operator of
                Neg -> (-x)
                Not -> error("Unsupported operator type.")
        (BoolVal bool) -> BoolVal (boolOp operation bool) where
            boolOp operator x = case operator of
                Not -> (not bool)
                Neg -> error("Unsopprted operator type.")
        (StringVal _) -> error("No unary operators supported for strings.")
        Nil -> error("Unsupported operand 'nil'.")
            

    where val = evaluate(op)
evaluate (Group expr) = evaluate(expr)
