{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-type-defaults -Wno-unused-matches -Wno-name-shadowing -Wno-unused-imports -Wno-compat-unqualified-imports -Wno-dodgy-imports -Wno-orphans -Wno-unused-local-binds  #-}

module Main (main) where

import System.IO
import Fetch
import Parse
import Database

import Control.Concurrent
import Control.Exception (try)
import Network.HTTP.Simple
import Text.Read


main :: IO ()
main = do
    putStrLn "---------------------------------"
    putStrLn "  Welcome to the Weather data app  "
    putStrLn "  (1) Download data              "
    putStrLn "  (2) Query by state     "
    putStrLn "  (3) Get list of random states     "
    putStrLn "  (4) Get list of Min/Max Temprature Stations     "
    putStrLn "  (5) Update average temperature of any station       "
    putStrLn "  (6) Store Station Details to a file          "
    putStrLn "  (7) Store Weather Details to a file          "
    putStrLn "  (8) Quit                     "
    putStrLn "---------------------------------"
    conn <- initialiseDB
    hSetBuffering stdout NoBuffering
    putStr "Choose an option > "
    maybeInt <- fmap readMaybe getLine :: IO (Maybe Int) -- Exception handling for Int type inputs
    case maybeInt of 
        Nothing -> do 
            putStrLn "Only Int input allowed"
            threadDelay 1000000
            main
        Just option -> do
            case option of
                1 -> do
                    let url = "https://corgis-edu.github.io/corgis/datasets/csv/weather/weather.csv"
                    putStrLn "Downloading..."
                    resp <- try $ download url  -- Excetption handling for http request
                    case resp of
                        Left e -> do 
                            print (e :: HttpException)
                            putStrLn "Please fix http request or try querying existing data"
                            main
                        Right response -> do
                            case (parseRecords $ getResponseBody response) of
                                Left e -> do
                                    print e
                                    putStrLn "Please fix http request or try querying existing data"
                                    main
                                Right recs -> do
                                    print "Saving on DB..."
                                    saveRecords conn recs
                                    putStrLn "Saved!"
                                    main
                2 -> do
                    entries <- queryByState conn
                    mapM_ print entries
                    main
                3 -> do
                    entries <- queryRandStations conn
                    mapM_ print entries
                    main
                4 -> do
                    response <- queryTopMinMaxTemperatureStations conn
                    mapM_ print response
                    main
                5 -> do
                    response <- updateLocationForStation conn
                    confirm_update <- queryByStation conn
                    mapM_ print confirm_update
                    main
                6 -> do
                    stationJsonParser conn
                    putStrLn "Download complete!"
                    threadDelay 1000000
                    main
                7 -> do
                    weatherJsonParser conn
                    putStrLn "Download complete!"
                    threadDelay 1000000
                    main
                8 -> putStrLn "Hope you've enjoyed using the app!"
                _ -> do
                    putStrLn "Please enter a valid number" -- Exception handling to make sure we enter a valid option number
                    threadDelay 1000000
                    main
