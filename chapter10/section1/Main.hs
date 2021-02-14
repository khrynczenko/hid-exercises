module Main where

import Criterion.Main

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

main :: IO ()
main = defaultMain [
      bench "sumN lazy" $ whnf sumN 100000
    , bench "sumN strict" $ whnf sumN' 100000
    ]

-- cabal bench
-- cabal bench --benchmark-options="--output stats.html"
