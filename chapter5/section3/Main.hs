module Main where

import Data.IORef
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
    

main :: IO ()
main = do
    s <- mainIORef
    putStrLn "You sum is: "
    print s
