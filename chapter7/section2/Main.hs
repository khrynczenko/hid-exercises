{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Reader
import Text.Read (readMaybe)
import Data.Text.Read (decimal)
import qualified Data.Text as T
import Data.Char

rpns :: [String]
rpns = [ "answer"
       , "12 13 + 1"
       , "2 +"
       , "x y +"
       , "1x +"
       , "1 22 1 22 0 2 * * * * *"
       , "10 1 2 + 2 2 1 2 * + * * * 1 x 2 + + +"]

type Stack = [Integer]
type EnvVars = [(String, Integer)]

data EvalError = NotEnoughElements
               | ExtraElements
               | NotANumber String
               | UnknownVar String
               deriving Show

type EvalM = ReaderT EnvVars (ExceptT EvalError (State Stack))

push :: Integer -> EvalM ()
push x = get >>= (\stack -> put (x : stack))

pop :: EvalM Integer
pop = get >>= pop'
  where
    pop' :: Stack -> EvalM Integer
    pop' [] = throwError NotEnoughElements
    pop' (x:xs) = put xs >> pure x

checkHasOneElement :: EvalM ()
checkHasOneElement = gets length >>= (\l -> when (l /= 1) (throwError ExtraElements))

readVar :: String -> EvalM Integer
readVar name = do
    var <- asks (lookup name)
    case var of
        Just n -> pure n
        Nothing -> throwError $ UnknownVar name

readNumber :: String -> EvalM Integer
readNumber txt = case decimal (T.pack txt) of
    Right (n, rest) | T.null rest -> pure n
    _ -> throwError $ NotANumber txt

reportEvalResults :: Either EvalError [String] -> String
reportEvalResults (Left e) = "Error: " <> show e 
reportEvalResults (Right b) = show b

evalRPNOnce :: String -> EvalM Integer
evalRPNOnce expr = clearStack >> traverse step (words expr) >> checkHasOneElement >> pop
  where
    clearStack = put []

    step :: String -> EvalM ()
    step "+" = processTops (+)
    step "-" = processTops (-)
    step "*" = processTops (*)
    step x
        | isLetter $ head x = readVar x >>= push
        | otherwise = readNumber x >>= push
    
    processTops op = flip op <$> pop <*> pop >>= push

evalRPNMany :: [String] -> EnvVars -> String
evalRPNMany txts env = reportEvalResults $
    evalState (runExceptT (runReaderT (mapM evalOnce txts) env)) []
  where
    evalOnce txt = (show <$> (evalRPNOnce txt)) `catchError` (pure . show)

main :: IO ()
main = putStrLn "Hello, Haskell!"
