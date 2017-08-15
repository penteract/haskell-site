{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
module PageApp where

import Prelude hiding(lookup,delete,insert)

import Data(GameStore)
import Template
import Tools

import Data.Char(toLower)
import Network.Wai
--import Network.HTTP.Types
import Network.HTTP.Types.Status
import Network.HTTP.Types.Method
import Network.HTTP.Types.Header
import qualified Data.Map as Map
import qualified Control.Concurrent.Map as CMap
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as CL
--import qualified Data.ByteString.Lazy as LB
import System.FilePath((</>))
--import Network.HTTP.Types()

type Handler = Request -> IO (Templates -> Response)
type Page = [(Method,Handler)]

homePage :: Handler
homePage _ = return $ "games.html" `loadWith` [("x",toValue ("hi"::String))]

loadWith :: FilePath -> [(Variable,Value)] -> Templates -> Response
loadWith path env ts = fromBoth $ do
    temp <- ts path ? debug ("template {} not found"%path)
    case runM (eval temp) (EvalConfig ts False) Map.empty of
        Left err -> Left $ debug err
        Right body -> return $ responseLBS ok200 [(hContentType,"text/html")] (CL.pack body)

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
pageApp ts m = pageApp' ts m $ lookIn ([] :: [(C.ByteString,Page)])

pageApp' :: Templates -> GameStore -> Lookup C.ByteString Page -> Application
pageApp' ts gs getPage req resp = either resp (\ a -> a req >>= resp.($ts)) $ do
    path == cannonicalPath ?? redirect cannonicalPath
    methodHandlers <- getPage path  ? pageNotFound
    lookup (requestMethod req) methodHandlers ? allow (map fst methodHandlers)
    where
        path = rawPathInfo req
        cannonicalPath = cannonise path
