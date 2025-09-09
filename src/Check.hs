{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE UndecidableInstances #-}
module Check(Check(..), CheckT(..), throwWarning) where
import Control.Monad.Except (MonadError(..))
import Control.Monad.IO.Class(MonadIO(..))
import Control.Monad.State.Class(MonadState(..))
import Control.Monad.Trans(MonadTrans(lift))

data Check e a
    = Checked a
    | Error e
    deriving Show

instance Functor (Check e) where
    fmap :: (a -> b) -> Check e a -> Check e b
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

newtype CheckT e m a = CheckT {runCheckT :: m (Check e a)}

instance (Functor m) => Functor (CheckT e m) where
    fmap :: (a -> b) -> CheckT e m a -> CheckT e m b
    fmap f (CheckT ma) = CheckT (fmap (fmap f) ma)

instance (Monad m) => Applicative (CheckT e m) where
    pure x = CheckT (return $ Checked x)
    CheckT mf <*> CheckT ma = CheckT $ do
        f <- mf
        a <- ma
        return (f <*> a)

instance (Monad m) => Monad (CheckT e m) where
    return = pure
    CheckT ma >>= f = CheckT $ do
        a <- ma
        case a of
            Error e -> return (Error e)
            Checked x -> runCheckT (f x)

instance (Monad m) => MonadError e (CheckT e m) where
    throwError e = CheckT $ return (Error e)
    CheckT ma `catchError` handler = CheckT $ do
        a <- ma
        case a of 
            Error e -> runCheckT (handler e)
            Checked x -> return (Checked x)
            

instance (MonadIO m) => MonadIO (CheckT e m) where
    liftIO :: IO a -> CheckT e m a
    liftIO io = CheckT $ do
        x <- liftIO io
        return (Checked x)

instance MonadTrans (CheckT e) where
    lift :: Monad m => m a -> CheckT e m a
    lift ma = CheckT $ (Checked <$> ma)


instance MonadState s m => MonadState s (CheckT e m) where
    get :: CheckT e m s
    get = lift get
    put :: s -> CheckT e m ()
    put s = lift (put s)

throwWarning :: String -> CheckT e IO ()
throwWarning = liftIO . putStrLn