{-# OPTIONS_GHC -Wno-type-defaults -Wno-unused-matches -Wno-name-shadowing -Wno-unused-imports -Wno-compat-unqualified-imports -Wno-dodgy-imports -Wno-orphans -Wno-unused-local-binds  #-}
{-|
Module      : Haskell Weather Project
Description : Fetch data from URL
Maintainer  : Shivani, Raj, Sohail & Sarthak
-}


module Fetch (
    download
) where

import qualified Data.ByteString.Lazy.Char8 as L8
import Network.HTTP.Simple
import Data.Typeable

type URL = String


{-|
>Fetch Data from web 🤲🏻
-}
download :: URL -> IO (Response L8.ByteString)
download url = do
    request <- parseRequest url
    response <- httpLBS request
    return $ response