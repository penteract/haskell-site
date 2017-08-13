{-# LANGUAGE OverloadedStrings #-}
module PageApp where

import Data(GameStore)
import Template(Template)

import Data.Char(toLower)

import Network.Wai
import Network.HTTP.Types
import qualified Control.Concurrent.Map as Map
--import Network.HTTP.Types


cannonise :: String -> String
cannonise = map toLower

pageApp :: GameStore -> Application
pageApp m = pageApp' m [ ]
--respond $ responseLBS status400 [] "Not a WebSocket request"

pageApp' :: GameStore -> [(String,Template)] -> Application
pageApp' gs ts req resp = resp $ responseLBS
        status200
        [("Content-Type", "text/plain")]
        "Hello, Web!"
