module Value(Value(..), stringifyVal) where

data Value
    = BoolVal Bool
    | NumVal Double
    | StringVal String
    | Nil
    deriving (Show, Eq)

stringifyVal :: Value -> String
stringifyVal (NumVal num) = show num
stringifyVal (BoolVal bool) = show bool
stringifyVal (StringVal str) = str
stringifyVal Nil = "nil"
