{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Control.Applicative
import Control.Monad.Trans.Class
import Control.Monad.State.Class

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }
newtype ReaderT r m a = ReaderT { runReaderT :: (r -> m a)}

instance Functor m => Functor (MaybeT m) where
    fmap :: (a -> b) -> MaybeT m a -> MaybeT m b
    fmap f (MaybeT r) = MaybeT {runMaybeT = fmap (fmap f) r}

instance Applicative m => Applicative (MaybeT m) where
    pure :: a -> MaybeT m a
    pure x =  MaybeT { runMaybeT = pure $ pure x }

    (<*>) :: MaybeT m (a -> b) -> MaybeT m a -> MaybeT m b
    (<*>) (MaybeT mmf) (MaybeT mma) = MaybeT ((fmap (<*>) mmf) <*> mma)

instance Monad m => Monad (MaybeT m) where
    (>>=) :: MaybeT m a -> (a -> MaybeT m b) -> (MaybeT m b)
    (>>=) (MaybeT mma) f = MaybeT $ mma >>=
        (\x -> case x of
            Just v -> runMaybeT $ f v
            Nothing -> (pure $ Nothing))

instance MonadTrans MaybeT where
    lift :: Monad m => m a -> MaybeT m a
    lift m1 = MaybeT { runMaybeT = fmap Just m1 }

instance MonadState s m => MonadState s (MaybeT m) where
    state = lift . state

instance Monad m => MonadFail (MaybeT m) where
    fail :: String -> MaybeT m a
    fail _ = MaybeT (pure Nothing)

instance Applicative m => Alternative (MaybeT m) where
    empty :: MaybeT m a
    empty = MaybeT (pure empty)

    (<|>) :: MaybeT m a -> MaybeT m a -> MaybeT m a
    (<|>) (MaybeT m1) (MaybeT m2) = MaybeT ((<|>) <$> m1 <*> m2)

instance MonadIO m => MonadIO (MaybeT m) where
    liftIO :: IO a -> MaybeT m a
    liftIO iom = (lift . liftIO) iom

main :: IO ()
main = putStrLn "Hello, Haskell!"
