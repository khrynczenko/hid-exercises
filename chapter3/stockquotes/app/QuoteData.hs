{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module QuoteData where

import GHC.Generics (Generic)

import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import Data.ByteString.Char8 (unpack)
import Data.Csv (decodeByName, FromNamedRecord, FromField (..))
import Data.Time (Day, parseTimeM, defaultTimeLocale)

instance FromField Day where
    parseField field =
        parseTimeM False defaultTimeLocale "%Y/%m/%d" $ unpack field

data QuoteData = QuoteData {
    day :: Day,
    volume :: Double,
    open :: Double,
    close :: Double,
    high :: Double,
    low :: Double
} deriving (Generic, FromNamedRecord, Show)

data QField = Open | Close | High | Low | Volume
    deriving (Eq, Ord, Show, Enum, Bounded)


field2fun :: QField -> QuoteData -> Double
field2fun Open quoteData = open quoteData
field2fun Close quoteData  = close quoteData
field2fun High quoteData = high quoteData
field2fun Low quoteData  = low quoteData
field2fun Volume quoteData = volume quoteData

readQuotes :: FilePath -> IO [QuoteData]
readQuotes filePath = do
    csvData <- BL.readFile filePath
    case decodeByName csvData of
        Left err -> do
            putStrLn err
            pure []
        Right (_h, row) -> pure $ foldl (flip (:)) [] row
