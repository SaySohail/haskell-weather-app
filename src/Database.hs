{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-type-defaults -Wno-unused-matches -Wno-name-shadowing -Wno-unused-imports -Wno-compat-unqualified-imports -Wno-dodgy-imports -Wno-orphans -Wno-unused-local-binds  #-}
{-|
Module      : Haskell Weather Project
Description : Create tables, insert records and query from DB
Maintainer  : Shivani, Raj, Sohail & Sarthak
-}

-- or, on GHCI:
-- > :set -XOverloadedStrings

module Database (
    initialiseDB,
    getOrCreateStation,
    saveRecords,
    createRecord,
    queryGetAllStations,
    --queryCountryAllEntries,
    queryRandStations,
    queryByState,
    queryTopMinMaxTemperatureStations,
    updateLocationForStation,
    queryByStation,
    queryGetWeatherDetails
    --queryCountryTotalCases
) where

import Types
import Database.SQLite.Simple
import Data.Vector


{-|
>Initialise DB and create tables
-}
initialiseDB :: IO Connection -- ^ IO Connection return type, which returns a connection to the database
initialiseDB = do
        conn <- open "weather.sqlite"
        execute_ conn "CREATE TABLE IF NOT EXISTS station_details (\
            \id INTEGER PRIMARY KEY AUTOINCREMENT,\
            \station VARCHAR(80) NOT NULL, \
            \location VARCHAR(50) NOT NULL, \
            \state VARCHAR(50) DEFAULT NULL \
            \)"
        execute_ conn "CREATE TABLE IF NOT EXISTS weather_details (\
            \precipitation FLOAT, \
            \avg_temp INTEGER, \
            \max_temp INTEGER, \
            \min_temp INTEGER, \
            \wind_direction INTEGER, \
            \wind_speed FLOAT, \
            \fk_station INTEGER\
            \)"
        return conn


{-|
> Create or Read from Station Table
-}

getOrCreateStation :: Connection -- ^ Connection Argument, to get the connection to the database
                   -> String -- ^ String Argument, containing the station name
                   -> String -- ^ String Argument, containing the location
                   -> String -- ^ String Argument, containing the state
                   -> IO Station -- ^ IO Station return type.
getOrCreateStation conn station location state = do
    results <- queryNamed conn "SELECT * FROM station_details WHERE station=:station AND state=:state" [":station" := station, ":state" := state]    
    --print resultsS
    if Prelude.length results > 0 then
        return . Prelude.head $ results
    else do
        execute conn "INSERT INTO station_details (station, location, state) VALUES (?, ?, ?)" (station, location, state)
        getOrCreateStation conn station location state

{-|
    > Get or Create Station
    > Use Primary Key of Station as Foreign Key of Weather
    > Insert into weather_details table
-}

createRecord :: Connection -- ^ Connection Argument, to get the connection to the database
             -> EachRecord -- ^ EachRecord Argument, containing the weather/station details to be inserted in the database
             -> IO () -- ^ IO return type.
createRecord conn eachrecord = do
    c <- getOrCreateStation conn (city eachrecord) (location eachrecord) (state eachrecord)
    let weather_record = WeatherRecord {
        precipitation_ = precipitation eachrecord,
        avg_temp_ = avg_temp eachrecord,
        max_temp_ = max_temp eachrecord,
        min_temp_ = min_temp eachrecord,
        wind_direction_ = wind_direction eachrecord,
        wind_speed_ = wind_speed eachrecord,
        fk_station = id_ c
    }
    execute conn "INSERT INTO weather_details VALUES (?,?,?,?,?,?,?)" weather_record

{-|
    > After downloading data from web we are saving the vector record into DB
-}
saveRecords :: Connection  -- ^ Connection Argument, to get the connection to the database
            -> Vector EachRecord -- ^ Vector EachRecord Argument, containing the weather/station details to be saved
            -> IO () -- ^ IO return type.
saveRecords conn = Data.Vector.mapM_ (createRecord conn)

{-|
    > Find 10 random stations
-}
queryRandStations :: Connection -- ^ Connection Argument, to get the connection to the database
                  -> IO[Station] -- ^ IO [Station] return type
queryRandStations conn = do
    putStr "Selection 10 random stations"
    query_ conn "SELECT * from station_details order by RANDOM() LIMIT 10"


{-|
    > Find all stations
-}
queryGetAllStations :: Connection -- ^ Connection Argument, to get the connection to the database
                    -> IO [Station]  -- ^ IO [Station] return type.
queryGetAllStations conn = do
    -- putStr "Enter station name > "
    -- stationName <- getLine
    -- putStrLn $ "Looking for " Prelude.++ stationName Prelude.++ " entries..."
    let sql = "SELECT id, station, location, state FROM station_details"
    query_ conn sql

{-|
    > Find weather details by Station ID
-}
queryByStation :: Connection -- ^ Connection Argument, to get the connection to the database
               -> IO [Response]  -- ^ IO [Response] return type.
queryByStation conn = do
    putStr "To verify update, please enter station name > "
    stationName <- getLine
    putStrLn $ "Looking for " Prelude.++ stationName Prelude.++ " station..."
    let sql = "SELECT id, station, location, state, precipitation, avg_temp, max_temp, min_temp, wind_direction, wind_speed FROM weather_details wd inner join station_details sd on wd.fk_station == sd.id where sd.station=? limit 1"
    query conn sql [stationName]

{-|
    > Find weather details by State
-}
queryByState :: Connection -- ^ Connection Argument, to get the connection to the database
             -> IO [Response]  -- ^ IO [Response] return type.
queryByState conn = do
    putStr "Enter state name > "
    stateName <- getLine
    putStrLn $ "Looking for " Prelude.++ stateName Prelude.++ " entries..."
    let sql = "SELECT id, station, location, state, precipitation, avg_temp, max_temp, min_temp, wind_direction, wind_speed FROM station_details sd inner join weather_details wd on wd.fk_station == sd.id where sd.state=? limit 10"
    query conn sql [stateName]

{-|
    > Find mininum and maximum temperature of stations
-}
queryTopMinMaxTemperatureStations :: Connection -- ^ Connection Argument, to get the connection to the database
                                  -> IO [Response] -- ^ IO[Response] return type.
queryTopMinMaxTemperatureStations conn = do
    putStr "Enter which temperature order to show: min/max :  > "
    tempOrder <- getLine
    putStrLn $ "Looking for " Prelude.++ tempOrder Prelude.++ " temperature entries..."
    --let tempOrderType = if tempOrder == "min" then "asc" else "desc"
    let sql = if tempOrder == "min"
                then "SELECT  id, station, location, state, precipitation, avg_temp, max_temp, min_temp, wind_direction, wind_speed FROM station_details sd inner join weather_details wd on wd.fk_station == sd.id order by wd.min_temp asc limit 10"
              else "SELECT id, station, location, state, precipitation, avg_temp, max_temp, min_temp, wind_direction, wind_speed FROM station_details sd inner join weather_details wd on wd.fk_station == sd.id order by wd.max_temp desc limit 10"
    query_ conn sql

{-|
    > Update Average Temperature of a station
-}
updateLocationForStation :: Connection -- ^ Connection Argument, to get the connection to the database
                         -> IO () -- ^ IO return type.
updateLocationForStation conn = do
    putStr "Enter station name > "
    stationName <- getLine
    putStr "Enter new average temperature: > "
    avgTemp <- getLine
    putStrLn $ "Updating average temperature for " Prelude.++ stationName Prelude.++ " station to " Prelude.++ avgTemp
    let sql = "UPDATE weather_details SET avg_temp = :avgtemp where fk_station=(Select id from station_details where station = :station limit 1)"
    executeNamed conn sql [":avgtemp" := avgTemp, ":station" := stationName]

{-|
    > Extract all records from weather_details table
-}
queryGetWeatherDetails :: Connection -- ^ Connection Argument, to get the connection to the database
                       -> IO [WeatherRecord] -- ^ IO [WeatherRecord] return type.
queryGetWeatherDetails conn = do
    -- putStr "Enter station name > "
    -- stationName <- getLine
    -- putStrLn $ "Looking for " Prelude.++ stationName Prelude.++ " entries..."
    let sql = "SELECT precipitation, avg_temp, max_temp, min_temp, wind_direction, wind_speed, fk_station  FROM weather_details"
    query_ conn sql

-- docs
