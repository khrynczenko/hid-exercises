module Main where

main :: IO ()
main = readFile "does-not-exist.txt" >>= putStrLn -- throws exception
