{-# LANGUAGE OverloadedStrings #-}
module PageApp where

import Data(GameStore)
import Template
import Tools

import Data.Char(toLower)
import Network.Wai
--import Network.HTTP.Types
import Network.HTTP.Types.Status
import Network.HTTP.Types.Method
import Network.HTTP.Types.Header
import qualified Control.Concurrent.Map as Map
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as CL
--import qualified Data.ByteString.Lazy as LB
import System.FilePath((</>))
--import Network.HTTP.Types()


type PureApp = Request -> Response
type Handler = Request -> IO (Templates -> Response)
type Page = (C.ByteString,[(Method,Handler)])

homePage :: Handler
homePage _ = return $ "games.html" `loadWith` ([("x","hi")] :: [(String,String)])

loadWith :: Valueable a => FilePath -> [(Variable,a)] -> Templates -> Response
loadWith path env ts = fromBoth $ do
    temp <- lookup path ts ? debug ("template {} not found"%path)

    return $ responseLBS ok200 [(hContentType,"text/html")] ""

--cannonise :: String -> String
cannonise = C.map toLower


debug :: String -> Response
debug = responseLBS internalServerError500 [("Content-Type","text/plain")]
    . CL.pack

pageNotFound :: Response
pageNotFound = responseFile notFound404 [("Content-Type","text/html")]
    ("staticfiles" </> "404.html") Nothing

redirect :: C.ByteString -> Response
redirect url = responseLBS permanentRedirect308 [(hLocation,url)] ""

allow :: [Method] -> Response
allow methods = responseFile methodNotAllowed405
    [(hAllow, C.intercalate ", " methods),("Content-Type","text/html")]
    ("staticfiles" </> "405.html") Nothing

pageApp :: Templates -> GameStore -> Application
pageApp ts m = pageApp' ts m [ ]

pageApp' :: Templates -> GameStore -> [Page] -> Application
pageApp' ts gs pages req resp = either resp (\ a -> a req >>= resp.($ts)) $ do
    path == cannonicalPath ?? redirect cannonicalPath
    methodHandlers <- lookup path pages ? pageNotFound
    lookup (requestMethod req) methodHandlers ? allow (map fst methodHandlers)
    where
        path = rawPathInfo req
        cannonicalPath = cannonise path
