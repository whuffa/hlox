module Check (Check(..), catchC) where

data Check a
    = Checked a
    | Error String
    deriving (Show, Eq)

thenC :: Check a -> (a -> Check b) -> Check b
thenC val f = val >>= f

returnC :: a -> Check a
returnC = return

failC :: String -> Check a
failC err = Error err

catchC :: Check a -> (String -> Check a) -> Check a
catchC val k = case val of
    Checked a -> Checked a
    Error e -> k e

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
