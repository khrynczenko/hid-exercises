{-# LANGUAGE NoImplicitPrelude #-}

module Data.Deque (Deque, empty, isEmpty, front, back, push_front) where

import GHC.Err (undefined)
import Data.Sequence hiding (empty)
import qualified Data.Sequence as Seq
import Data.Bool (Bool(..))
import Data.Maybe (Maybe(..))
import Prelude ((-))

newtype Deque a = Deque (Seq a)

empty :: Deque a
empty = Deque Seq.empty

isEmpty :: Deque a -> Bool
isEmpty (Deque seq) = null seq
front :: Deque a -> Maybe a
front (Deque seq) = Seq.lookup 0 seq
back :: Deque a -> Maybe a
back (Deque seq) = Seq.lookup ((Seq.length seq) - 1) seq
push_front :: a -> Deque a -> Deque a
push_front value (Deque seq) = Deque (value Seq.:<| seq)
