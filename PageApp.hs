{-# LANGUAGE OverloadedStrings #-}
module PageApp where

import Data(GameStore)
import Template(Template)
import Tools

import Data.Char(toLower)
import Network.Wai
--import Network.HTTP.Types
import Network.HTTP.Types.Status
import Network.HTTP.Types.Method
import Network.HTTP.Types.Header
import qualified Control.Concurrent.Map as Map
import qualified Data.ByteString.Char8 as C
import System.FilePath((</>))
--import Network.HTTP.Types()


type PureApp = Request -> Response
type Handler = Request -> IO Response
type Page = (C.ByteString,[(Method,Handler)])

homePage :: Handler
homePage = undefined

--cannonise :: String -> String
cannonise = C.map toLower

pageNotFound :: Response
pageNotFound = responseFile notFound404 [("Content-Type","text/html")]
    ("staticfiles" </> "404.html") Nothing

redirect :: C.ByteString -> Response
redirect url = responseLBS permanentRedirect308 [(hLocation,url)] ""

allow :: [Method] -> Response
allow methods = responseFile methodNotAllowed405
    [(hAllow, C.intercalate ", " methods),("Content-Type","text/html")]
    ("staticfiles" </> "405.html") Nothing

pageApp :: GameStore -> Application
pageApp m = pageApp' m [ ]

pageApp' :: GameStore -> [Page] -> Application
pageApp' gs pages req resp = either resp (\ a -> a req >>= resp) $ do
    path == cannonicalPath ?? redirect cannonicalPath
    methodHandlers <- lookup path pages ? pageNotFound
    lookup (requestMethod req) methodHandlers ? allow (map fst methodHandlers)
    where
        path = rawPathInfo req
        cannonicalPath = cannonise path
