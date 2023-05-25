{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-type-defaults -Wno-unused-matches -Wno-name-shadowing -Wno-unused-imports -Wno-compat-unqualified-imports -Wno-dodgy-imports -Wno-orphans -Wno-unused-local-binds  #-}
{-|
Module      : Haskell Weather Project
Description : Parse CSV to Haskell. Haskell to JSON. Save to file. 
Maintainer  : Shivani, Raj, Sohail & Sarthak
-}

module Parse (
    parseRecords,
    stationJsonParser,
    weatherJsonParser,
    writeToExistingFile,
    writeToNewFile,
    removeIfExists
) where

import Types
import Data.Vector
import Data.Csv
import qualified Data.ByteString.Lazy as BL
import Database.SQLite.Simple
import Database
import Data.Aeson
import Data.Aeson.Types 
import Data.ByteString.Lazy.Char8 as BC
import System.IO.Unsafe
import System.Directory
import Data.List
import Prelude hiding (catch)
import Control.Exception
import System.IO.Error hiding (catch)

-- | > The parseRecords function parses the downloaded csv file into Haskell data types.
parseRecords :: BL.ByteString -- ^ The Data.ByteString.Lazy ByteString Argument
             -> Either String (Vector EachRecord) -- ^ The return value
parseRecords j = Data.Csv.decode HasHeader j :: Either String (Vector EachRecord)


instance ToJSON Station

instance ToJSON WeatherRecord

-- | > The removeIfExists function removes the file if it exists in the path
removeIfExists :: FilePath -- ^ The file path argument 
               -> IO () -- ^ The return value
removeIfExists filepath = removeFile filepath

-- | > The Station function fetches the station details from database, parses the recieved details and stores them to a file.
stationJsonParser :: Connection -- ^ The connection argument, for connecting to the database
                  -> IO () -- ^ The return value
stationJsonParser conn = do
    let stations = queryGetAllStations conn
    let dataArray = unsafePerformIO stations
    let valueToPrint = Prelude.head dataArray
    let jsonValue = Data.Aeson.encode dataArray
    do
         files <- getDirectoryContents "downloaded_json/"
         let filtered = Prelude.filter (Data.List.isPrefixOf "station") files
         if Prelude.length filtered > 0 then
            writeToExistingFile filtered jsonValue
         else do
            writeToNewFile "station.json" jsonValue

-- | > The writeToExisitingFile function writes the JSON data to an exisiting file in the specified directory
writeToExistingFile :: [String] -- ^ The [String] argument, containing the file paths
                    -> ByteString   -- ^ The ByteString argument, containing the json data in ByteString
                    -> IO () -- ^ The return value
writeToExistingFile filteredList jsonValue = do
    let headOfFile = Prelude.head filteredList
    let filepath = "downloaded_json/" Prelude.++ headOfFile
    let stringValues = BC.unpack jsonValue
    Prelude.writeFile filepath stringValues
    print ("Haskell Data Added to the file: " Prelude.++ filepath)

-- | > The writeToNewFile function writes the JSON to a new file in the specified directory
writeToNewFile :: String -- ^ The String argument, containing the name of the file
               -> ByteString -- ^ The ByteString argument, containg the json data in ByteString
               -> IO ()
writeToNewFile nameOfFile jsonValue = do
    let filepath  = "downloaded_json/" Prelude.++ nameOfFile
    print filepath
    let stringValues = BC.unpack jsonValue
    Prelude.writeFile filepath stringValues
    print ("Haskell Data Added to the file: " Prelude.++ filepath)

-- | > The weatherJsonParser function fetches the Weather details from database, parses the recieved details and stores them to a file.
weatherJsonParser :: Connection -- ^ The connection argument, for connecting to the database
                  -> IO ()      -- ^ The return value
weatherJsonParser conn = do
    let weatherDetails = queryGetWeatherDetails conn
    let dataArray = unsafePerformIO weatherDetails
    let valueToPrint = Prelude.head dataArray
    let jsonValue = Data.Aeson.encode dataArray
    do
         files <- getDirectoryContents "downloaded_json/"
         let filtered = Prelude.filter (Data.List.isPrefixOf "weather") files
         if Prelude.length filtered > 0 then
            writeToExistingFile filtered jsonValue
         else do
            writeToNewFile "weather.json" jsonValue
