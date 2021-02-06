module Main where

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Text.Read (readMaybe)

type Stack = [Integer]
type EvalM = StateT Stack Maybe

push :: Integer -> EvalM ()
push x = get >>= (\stack -> put (x : stack))

pop :: EvalM Integer
pop = (not <$> isEmpty) >>= guard >> (get >>= (\stack -> put (tail stack) >> pure (head stack)))

isEmpty :: EvalM Bool
isEmpty = get >>= (pure . null)

hasOnlyOneElementOnStack :: EvalM ()
hasOnlyOneElementOnStack = ((== 1) . length)  <$> get >>= guard

readSafe :: (Read a, Alternative m) => String -> m a
readSafe str =
    case readMaybe str of
        Nothing -> empty
        Just n -> pure n

evalRPN :: String -> Maybe Integer
evalRPN expr = evalStateT evalRPN' []
  where
    evalRPN' :: EvalM Integer
    evalRPN' = traverse step (words expr) >> hasOnlyOneElementOnStack >> pop

    step :: String -> EvalM ()
    step "+" = processTops (+)
    step "-" = processTops (-)
    step "*" = processTops (*)
    step n = push (read n)
    
    processTops op = flip op <$> pop <*> pop >>= push

main :: IO ()
main = putStrLn "Hello, Haskell!"
