{-# LANGUAGE InstanceSigs #-}
module Main where

import Control.Monad.State
import System.Random

type Counter = State Int

doSomething :: Counter String
doSomething = state (\state -> ("Welcome", state)) >> put 2 >> doSomethingAgain

doSomethingAgain :: Counter String
doSomethingAgain = state (\state -> ("Welcome2", state + 1))

--main :: IO ()
--main = do
    --let (result, counts) = runState doSomething 1
    --(putStrLn . show) counts
    --putStrLn result


data Weapon = Rock | Paper | Scissors deriving (Show, Bounded, Enum, Eq)
data Winner = First | Second | Draw deriving (Show, Enum, Eq)

winner :: (Weapon, Weapon) -> Winner
winner (Paper, Rock) = First
winner (Scissors, Paper) = First
winner (Rock, Scissors) = First
winner (w1, w2)
    | w1 == w2 = Draw
    | otherwise = Second

instance Random Weapon where
    randomR :: RandomGen g => (Weapon, Weapon) -> g -> (Weapon, g)
    randomR (weaponLow, weaponHigh) gen = (toEnum num, newGen)
        where
            (num, newGen) = randomR (fromEnum weaponLow, fromEnum weaponHigh) gen
    random gen = randomR (minBound, maxBound) gen

gameRound :: State StdGen (Weapon, Weapon)
gameRound = do
    gen <- get
    let (w1, gen1) = random gen
    let (w2, gen2) = random gen1
    put gen2
    pure (w1, w2)

main :: IO ()
main = do
    let gen = mkStdGen 1
    let (result, newGen) = runState gameRound gen
    let (result2, _) = runState gameRound newGen
    putStrLn $ show $result
    putStrLn $ show $result2
