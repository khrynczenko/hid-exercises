{-# LANGUAGE DeriveAnyClass #-}

module Main where

import Control.Exception
import Control.Monad.Catch (MonadThrow, throwM)

data MyArithException = DivByZero | OtherArithException deriving (Show, Exception)

divPure :: Int -> Int -> Int
divPure _ 0 = throw DivByZero
divPure a b = a `div` b

divIO :: Int -> Int -> IO Int
divIO _ 0 = throwIO DivByZero
divIO a b = pure $ a `div` b

divM :: MonadThrow m => Int -> Int -> m Int
divM _ 0 = throwM DivByZero
divM a b = pure $ a `div` b

testComputation a b c = divM a b >>= divM c :: Maybe Int

main :: IO ()
main = putStrLn "Hello, Haskell!"
