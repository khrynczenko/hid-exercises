{-# LANGUAGE BangPatterns #-}

module Main where

sumN :: Int -> Int
sumN n = go 0 n
  where
    go acc 0 = acc
    go acc n = go (acc + n) (n - 1)

sumN' :: Int -> Int
sumN' n = go 0 n
  where
    go acc 0 = acc
    go acc n = acc `seq` go (acc + n) (n - 1)

sumN'' :: Int -> Int
sumN'' n = go 0 n
  where
    go acc 0 = acc
    go !acc n = go (acc + n) (n - 1)

data Point = Point { x :: !Double, y :: !Double } -- forces WHNF

data Shape = Rectangle {-# UNPACK #-} !Int
                       {-# UNPACK #-} !Int
           | Circle {-# UNPACK #-} !Int


main :: IO ()
main = putStrLn "Hello, Haskell!"
