module Variable(Variable(..)) where

data Variable a
    = Defined a
    | Declared
    | Undeclared
    deriving (Show)