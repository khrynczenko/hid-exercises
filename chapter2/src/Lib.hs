{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}

module Lib where

import Control.Monad
import Data.List

import System.Random

import Fmt

data Direction = North | East | South | West 
    deriving (Eq, Enum, Ord, Bounded, CyclicEnum, Read, Show)
data Turn = TNone| TLeft| TRight | TAround 
    deriving (Eq, Enum, Ord, Bounded, Read, Show)

class (Eq a, Enum a, Bounded a) => CyclicEnum a where
    cpred :: a -> a
    cpred value
        | value == minBound = maxBound
        | otherwise = pred value
    
    csucc :: a -> a
    csucc value
        | value == maxBound = minBound
        | otherwise = succ value

instance Semigroup Turn where
    TNone <> TNone = TNone
    TLeft <> TLeft = TAround
    TRight <> TRight = TAround
    TLeft <> TRight = TNone
    TRight <> TLeft = TNone
    TLeft <> TAround = TRight
    TAround <> TLeft = TRight
    TRight <> TAround = TLeft
    TAround <> TRight = TLeft
    TAround <> TAround = TNone
    TNone <> turn = turn
    turn <> TNone = turn

instance Monoid Turn where
    mempty = TNone

instance Buildable Direction where
    build North = "N"
    build East = "E"
    build South = "S"
    build West = "W"

instance Buildable Turn where
    build TNone = "None"
    build TLeft = "Left"
    build TRight = "Right"
    build TAround = "Around"

instance Random Direction where
    randomR (lo, hi) g = (toEnum i, g')
        where (i, g') = randomR (fromEnum lo, fromEnum hi) g
    random gen = randomR (minBound, maxBound) gen

instance Random Turn where
    randomR (low, high) gen = (toEnum value, newGen)
        where 
            (value, newGen) = randomR (fromEnum low, fromEnum high) gen
    random gen = randomR (minBound, maxBound) gen

randomsIO :: Random a => Int -> IO [a]
randomsIO n = replicateM n randomIO

randomTurns :: Int -> IO [Turn]
randomTurns n = randomsIO n

randomDirections :: Int -> IO [Direction]
randomDirections n = randomsIO n

writeRandomFile :: (Random a, Show a) => Int -> (Int -> IO [a]) -> FilePath -> IO () 
writeRandomFile n gen fname = do
    xs <- gen n
    writeFile fname $ unlines $ map show xs

every :: (Enum a, Bounded a) => [a]
every = enumFrom minBound

rotate :: Turn -> Direction -> Direction
rotate TNone direction = direction
rotate TLeft direction = cpred direction
rotate TRight direction = csucc direction
rotate TAround direction = (csucc . csucc) direction

orient :: Direction -> Direction -> Turn
orient d1 d2 =
    head $ filter (\turn -> rotate turn d1 == d2) every


-- |Given a starting direction and a list of turns
-- produce a final direction that would be present after
-- performing all of the turns.
rotateMany :: Direction -> [Turn] -> Direction
rotateMany direction turns = rotate (mconcat turns) direction

-- |Given a starting direction and a list of turns
-- produce a list of directions that would result
-- by aplying each turn.
rotateManyWithSteps :: Direction -> [Turn] -> [Direction]
rotateManyWithSteps direction turns = scanl (flip rotate) direction turns

-- |Given the list of directions produce the list of
-- turns that have to be performed to orient in these directions.
orientMany :: [Direction] -> [Turn]
orientMany directions@(_:_:_) = zipWith orient directions (tail directions)
orientMany _ = []
