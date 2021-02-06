module Rpm where

import Control.Monad.State

type Stack = [Integer]
type EvalM = State Stack

push :: Integer -> EvalM ()
push x = get >>= (\stack -> put (x : stack))

pop :: EvalM Integer
pop = get >>= (\stack -> put (tail stack) >> pure (head stack))

isEmpty :: EvalM Bool
isEmpty = get >>= (pure . null)

notEmpty :: EvalM Bool
notEmpty = (not . null) <$> get

hasOnlyOneElementOnStack :: EvalM Bool
hasOnlyOneElementOnStack = ((== 1) . length) <$> get

evalRPN :: String -> Integer
evalRPN expr = evalState evalRPN' []
  where
    evalRPN' :: EvalM Integer
    evalRPN' = traverse step (words expr) >> pop

    step :: String -> EvalM ()
    step "+" = processTops (+)
    step "-" = processTops (-)
    step "*" = processTops (*)
    step n = push (read n)
    
    processTops op = flip op <$> pop <*> pop >>= push

