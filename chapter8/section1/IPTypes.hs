module IPTypes where

import Data.Word

newtype IP = IP { unIP :: Word32 } deriving (Eq, Ord, Show)

data IPRange = IPRange IP IP deriving Eq

newtype IPRangeDB = IPRangeDB [IPRange] deriving Eq
