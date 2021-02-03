module Main where

import Control.Monad.ST
import Control.Monad
import Data.IORef
import Data.STRef
import Text.Read

mainIORef :: IO Int
mainIORef = do
    s <- newIORef 0
    go s
  where
    go acc = readNumber >>= processNumber acc
    
    readNumber :: IO (Maybe Int)
    readNumber = do
        putStr "Put integer number (not a number to finish): "
        readMaybe <$> getLine

    processNumber acc Nothing = readIORef acc
    processNumber acc (Just n) = modifyIORef' acc (+n) >> go acc
    
--main :: IO ()
--main = do
    --s <- mainIORef
    --putStrLn "You sum is: "
    --print s

countZeroST :: [Int] -> ST s Int
countZeroST xs = do
    ref <- newSTRef 0
    forM_ xs (\x -> if x == 0 then modifySTRef ref (+1) else pure ())
    readSTRef ref

main :: IO ()
main = do
    print $ runST $ countZeroST [0,1,0,1,0]
