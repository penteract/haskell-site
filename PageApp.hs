{-# LANGUAGE OverloadedStrings #-}
module PageApp where

import Data(GameStore)
import Template
import Tools
import Pages

import Data.Char(toLower)
import Network.Wai
--Network.HTTP.Types does not currently export status308
import Network.HTTP.Types.Status
import Network.HTTP.Types.Method
import Network.HTTP.Types.Header
import qualified Data.Map as Map
import qualified Control.Concurrent.Map as CMap
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as CL
import System.FilePath((</>))

type Page = [(Method,Handler)]

--cannonise :: String -> String
cannonise = C.map toLower

--function for rearranging arguments while presenting a wai-style interface
pageApp :: Templates -> GameStore -> Application
pageApp ts gs req resp =
    pageApp' (`lookup` [("/",[(methodGet,homePage)])]) req gs >>= resp.($ts)

--Consider reordering arguments
pageApp' :: (C.ByteString -> Maybe Page) -> Handler
pageApp' getPage req = either (return.return.return) ($req) $ do
    path == cannonicalPath ?? redirect cannonicalPath
    methodHandlers <- getPage path ? pageNotFound
    lookup (requestMethod req) methodHandlers ? allow (map fst methodHandlers)
    where
        path = rawPathInfo req
        cannonicalPath = cannonise path

pageNotFound :: Response
pageNotFound = responseFile notFound404 [("Content-Type","text/html")]
    ("staticfiles" </> "404.html") Nothing

redirect :: C.ByteString -> Response
redirect url = responseLBS permanentRedirect308 [(hLocation,url)] ""

allow :: [Method] -> Response
allow methods = responseFile methodNotAllowed405
    [(hAllow, C.intercalate ", " methods),("Content-Type","text/html")]
    ("staticfiles" </> "405.html") Nothing
