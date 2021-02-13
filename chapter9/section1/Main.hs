{-# LANGUAGE MagicHash #-}

module Main where

import GHC.Prim

data Shape = Rectangle Int Int | Circle Int

data Shape2 = Rectangle2 Int# Int# | Circle2 Int#

main :: IO ()
main = putStrLn "Hello, Haskell!"
