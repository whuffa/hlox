module Value(Value(..), stringifyVal) where
import Data.List(stripPrefix)
data Value
    = BoolVal Bool
    | NumVal Double
    | StringVal String
    | Nil
    deriving (Show, Eq)

stripSuffix :: Eq a => [a] -> [a] -> Maybe [a]
stripSuffix suf xs =
  reverse <$> stripPrefix (reverse suf) (reverse xs)

stringifyVal :: Value -> String
stringifyVal (NumVal num) = case stripSuffix ".0" (show num) of
    Just trim -> trim
    Nothing -> (show num)
stringifyVal (BoolVal bool) = show bool
stringifyVal (StringVal str) = str
stringifyVal Nil = "nil"
