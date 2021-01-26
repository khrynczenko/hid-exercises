import Control.Monad
import Data.List
import System.Exit

import Lib

test_allTurnsInUse :: Bool
test_allTurnsInUse =
    sort (nub [orient d1 d2 | d1 <- every, d2 <- every]) == every

test_rotationsMonoidAgree :: [Turn] -> Bool
test_rotationsMonoidAgree turns =
    and [ rotateMany direction turns == rotateMany direction turns | direction <- every]

test_orientRotateAgree :: [Direction] -> Bool
test_orientRotateAgree [] = True
test_orientRotateAgree ds@(d:_) = ds == rotateManyWithSteps d (orientMany ds)

main :: IO ()
main = do
    directions <- randomDirections 1000
    turns <- randomTurns 1000
    when (not $ and 
        [ test_allTurnsInUse
        , test_orientRotateAgree directions
        , test_rotationsMonoidAgree turns
        ]) exitFailure
