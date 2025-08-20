module Check (Check(..)) where

data Check a
    = Checked a
    | Error String
    deriving (Show, Eq)

instance Monad (Check) where
    return = pure
    Checked x >>= f = f x
    Error err >>= _ = Error err
    --fail msg = Left (strMsg msg)
     

instance Functor Check where
    fmap f x = do
        mx <- x
        return (f mx)

instance Applicative Check where
    pure = Checked
    mf <*> mx = do
        f <- mf
        x <- mx
        return (f x)
