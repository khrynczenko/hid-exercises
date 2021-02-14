module Main where

import System.Environment
import System.TimeIt

isPrime :: Integer -> Bool
isPrime n = all notDivideBy [2 .. n - 1]
  where
    notDivideBy m = n `mod` m /= 0



main :: IO ()
main = getArgs >>= timeIt . print . isPrime . read . head
