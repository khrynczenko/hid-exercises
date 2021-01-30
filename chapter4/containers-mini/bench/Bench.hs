module Bench where

import Data.List (unfoldr)
import Data.Stack hiding (empty)
import Data.Stack as S
import System.TimeIt

add stack = push 1 stack

main :: IO ()
main = do
    timeItNamed "Stack" $ print $ add S.empty
