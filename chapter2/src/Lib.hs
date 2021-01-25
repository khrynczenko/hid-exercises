{-# LANGUAGE DeriveAnyClass #-}

module Lib where

data Direction = North | East | South | West deriving (Eq, Enum, Bounded, CyclicEnum, Show)
data Turn = TNone| TLeft| TRight | TAround deriving (Eq, Enum, Bounded, Show)

class (Eq a, Enum a, Bounded a) => CyclicEnum a where
    cpred :: a -> a
    cpred value
        | value == minBound = maxBound
        | otherwise = pred value
    
    csucc :: a -> a
    csucc value
        | value == maxBound = minBound
        | otherwise = succ value

--applyNTimes :: Int -> (a -> a) -> a -> a
--applyNTimes 0 _ value = value
--applyNTimes i f value = applyNTimes (i-1) (f value)

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
rotateMany direction turns = foldl (flip rotate) direction turns

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
