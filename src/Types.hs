{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-type-defaults -Wno-unused-matches -Wno-name-shadowing -Wno-unused-imports -Wno-compat-unqualified-imports -Wno-dodgy-imports -Wno-orphans -Wno-unused-local-binds  #-}
{-|
Module      : Haskell Weather Project
Description : Feed and load data from DB
Maintainer  : Shivani, Raj, Sohail & Sarthak
-}

module Types (
    EachRecord (..),
    WeatherRecord (..),
    Station (..),
    Response (..)
) where

import GHC.Generics (Generic)
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.ToRow
import Data.Csv

import Prelude hiding (filter)
import qualified Data.ByteString.Lazy as BL

data EachRecord = EachRecord {
    precipitation :: Float, -- ^ Precipitation
    date :: String,-- ^ Date
    month :: Int,-- ^ Month
    week :: Int,-- ^ Week
    year :: Int,-- ^ Year
    city :: String,-- ^ City
    code :: String,-- ^ Code
    location :: String,-- ^ Location
    state ::  String,-- ^ State
    avg_temp :: Int,-- ^ Avgerage Temperature
    max_temp :: Int,-- ^ Max Temperature
    min_temp :: Int,-- ^ Minimum Temperature
    wind_direction :: Int,-- ^ Wind Direction
    wind_speed :: Float-- ^ Wind Speed
} deriving (Show, Generic)

data WeatherRecord = WeatherRecord {
    precipitation_ :: Float,-- ^ Precipitation
    avg_temp_ :: Int,-- ^ Average Temperature
    max_temp_ :: Int,-- ^ Max Temperature
    min_temp_ :: Int,-- ^ Minimum Temperature
    wind_direction_ :: Int,-- ^ Wind Direction
    wind_speed_ :: Float,-- ^ Wind Speed
    fk_station :: Int-- ^ Foreign Key
} deriving (Show,Generic)

data Station = Station {
    id_ :: Int,-- ^ Primary Key
    station_ :: String,-- ^ Station
    location_ ::  String,-- ^ Location
    state_ :: String-- ^ State
} deriving (Show,Generic)

data Response = Response {
    id__ :: Int,-- ^ Primary Key
    station__ :: String,-- ^ Station
    location__ :: String,-- ^ Location
    state__ :: String,-- ^ State
    precipitation__ :: Float,-- ^ Precipitation
    avg_temp__ :: Int,-- ^ Average Temperature
    max_temp__ :: Int,-- ^ Maximum Temperature
    min_temp__ :: Int,-- ^ Minimum Temperature
    wind_direction__ :: Int,-- ^ Wind Direction
    wind_speed__ :: Float-- ^ Wind Speed
} deriving (Show)


instance FromRecord EachRecord

instance FromRow EachRecord where
    fromRow = EachRecord <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

instance ToRecord EachRecord where
    toRecord (EachRecord precipitation date month week year city code location state avg_temp max_temp min_temp wind_direction wind_speed) = record [
        toField precipitation ,toField date ,toField month ,toField week ,toField year ,toField city ,toField code ,toField location ,toField state ,toField avg_temp ,toField max_temp ,toField min_temp ,toField wind_direction ,toField wind_speed]

instance FromRow Station where
    fromRow = Station <$> field <*> field <*> field <*> field

instance ToRow Station where
    toRow (Station i station loc state)
        = toRow (i, station, loc, state)

instance FromRow WeatherRecord where
    fromRow = WeatherRecord <$> field <*> field <*> field <*> field <*> field <*> field <*> field

instance ToRow WeatherRecord where
    toRow (WeatherRecord p at mat mit wd ws fk_c) 
        = toRow (p, at, mat, mit, wd, ws, fk_c)

instance FromRow Response where
    fromRow = Response <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

instance ToRow Response where
    toRow (Response i station loc state p at mat mit wd ws) 
        = toRow (i, station, loc, state, p, at, mat, mit, wd, ws)