{-# LANGUAGE DeriveAnyClass #-}

module ParseIP where

import Control.Applicative
import Control.Monad
import Data.Word
import Text.Read

import Control.Monad.Catch
import qualified Data.Text as T

import IPTypes

type LineNumber = Int

newtype ParseError = ParseError LineNumber deriving (Show, Eq)

data InvalidArgsException = LoadIPRangesError ParseError
                           | InvalidIP String
                           deriving Exception

instance Show InvalidArgsException where
    show (LoadIPRangesError (ParseError idx)) =
        "Error loading ip range databases (line: " <> show idx ++ ")"
    show (InvalidIP ip) = "Invalid IP address to check: " <> ip

guarded :: Alternative f => (a -> Bool) -> a -> f a
guarded f a = if f a then pure a else empty

isLengthOf :: Int -> [a] -> Bool
isLengthOf n xs = length xs == n

buildIP :: [Word8] -> IP 
buildIP octets = (IP . fst) $ foldr go (0, 1) octets
  where
    go octet (s, k) = (s + fromIntegral octet * k, k*256)

--parseIP :: String -> Maybe IP
--parseIP text = guarded (isLengthOf 4) . T.unpack . T.splitOn "."
    -- >=> mapM (readMaybe >=> guarded fitsOctet)
    -- >=> (pure . buildIP) text
  --where
    --fitsOctet value = 0 <= value && value < 256

parseIP :: String -> Maybe IP
parseIP text = pure text 
    >>= guarded (isLengthOf 4) . fmap T.unpack . T.splitOn (T.pack ".") . T.pack
    >>= mapM (\x -> (readMaybe x :: Maybe Word8) >>= guarded fitsOctet)
    >>= (pure . buildIP)
  where
    fitsOctet value = 0 <= value && value < 256

parseIPRange :: String -> Maybe IPRange
parseIPRange text =
    pure text
    >>= guarded (isLengthOf 2) . fmap T.unpack . T.splitOn (T.pack ",") . T.pack
    >>= mapM parseIP
    >>= listToIPRange
  where
    listToIPRange [a, b] = pure (IPRange a b)
    listToIPRange _ = empty

parseIPRanges :: String -> Either ParseError IPRangeDB
parseIPRanges text = (fmap IPRangeDB . mapM parseLine . zip [1..] . lines) text
  where
    parseLine (ln, s) = case parseIPRange s of
        Nothing -> Left (ParseError ln)
        Just ipr -> Right ipr
