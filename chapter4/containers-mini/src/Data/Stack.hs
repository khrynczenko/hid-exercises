module Data.Stack (Stack, empty, isEmpty, push, pop, top) where

newtype Stack a = Stack [a] deriving (Show)

empty :: Stack a
empty = Stack []

push :: a -> Stack a -> Stack a
push value (Stack xs) = Stack (value : xs)

isEmpty :: Stack a -> Bool
isEmpty (Stack []) = True
isEmpty _ = False

pop :: Stack a -> Stack a
pop (Stack xs) = Stack (tail xs)

top :: Stack a -> a
top (Stack (x:_)) = x

