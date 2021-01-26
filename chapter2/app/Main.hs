{-# Language OverloadedStrings #-}
module Main where

import System.Environment

import Fmt

import Lib

rotateFromFile :: Direction -> FilePath -> IO ()
rotateFromFile dir fname = do
    f <- readFile fname
    let turns = map read $ lines f
        finalDir = rotateMany dir turns
        dirs = rotateManyWithSteps dir turns
    fmtLn $ "Final direction: " +|| finalDir ||+ ""
    fmt $ nameF "Intermediate directions" (unwordsF dirs)

orientFromFile :: FilePath -> IO ()
orientFromFile fname = do
    f <- readFile fname
    let turns = orientMany (map read $ lines f) :: [Turn]
    fmt $ nameF "Intermediate directions" (unwordsF turns)


main :: IO ()
main = do
    args <- getArgs
    case args of
        ["-r", fname, dir] -> rotateFromFile (read dir) fname
        ["-o", fname] -> orientFromFile fname
        _ -> putStrLn $ "Usage: locator -o filename\n" ++
                        "       locator -r filename direction"
