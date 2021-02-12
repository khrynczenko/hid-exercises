module Main where

import Test.Tasty
import Test.Tasty.Hspec

import ParseIPSpec


main :: IO ()
main = do
    specs <- concat <$> mapM testSpecs
        [ parseIPSpecs]
    defaultMain (testGroup "All tests" [testGroup "Specs" specs])
