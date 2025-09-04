{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module CheckN(Check(..)) where
import Control.Monad.Except (MonadError(..))

data Check e a
    = Checked a
    | Error e
    deriving Show

instance Functor (Check e) where
    fmap f (Checked x) = Checked (f x)
    fmap _ (Error y) = Error y

instance Applicative (Check e) where
    pure = Checked
    (Checked f) <*> (Checked x) = Checked (f x)
    (Error err) <*> _ = Error err
    _ <*> (Error err) = Error err

instance Monad (Check e) where
    return = pure
    Checked x >>= f = f x
    Error err >>= _ = Error err

instance MonadError e (Check e) where
    throwError = Error
    Error e `catchError` h = h e
    Checked a `catchError` _ = Checked a


