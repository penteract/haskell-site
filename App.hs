--{-# LANGUAGE OverloadedStrings #-}

import Network.Wai --
import Network.HTTP.Types
import Network.Wai.Handler.Warp (run)
import Network.Wai.Handler.WebSockets
import Network.WebSockets hiding(requestHeaders) --(defaultConnectionOptions,acceptRequest,sendTextData,ServerApp)
--import Data.Text(Text)

import PageApp(pageApp)
import WSApp(wsApp)
import Data (GameStore)
import Template (Templates)

import qualified Control.Concurrent.Map as Map

--import Template

appmm :: Middleware
appmm a1 req respond = do
    putStrLn $ unlines $ map ($req)  [
        ("requestMethod:"++).show.requestMethod,
        ("httpVersion:"++).show.httpVersion,
        ("rawPathInfo:"++).show.rawPathInfo,
        ("rawQueryString:"++).show.rawQueryString,
        ("requestHeaders:"++).show.requestHeaders,
        ("isSecure:"++).show.isSecure,
        ("remoteHost:"++).show.remoteHost,
        ("pathInfo:"++).show.pathInfo,
        ("queryString:"++).show.queryString,
        ("requestBodyLength:"++).show.requestBodyLength,
        ("requestHeaderHost:"++).show.requestHeaderHost,
        ("requestHeaderRange:"++).show.requestHeaderRange,
        ("requestHeaderReferer:"++).show.requestHeaderReferer,
        ("requestHeaderUserAgent:"++).show.requestHeaderUserAgent]
    a1 req respond
    {-respond $ responseLBS
        status200
        [("Content-Type", "text/plain")]
        "Hello, Web!"-}

main :: IO ()
main = do
    m <- Map.empty
    putStrLn "http://localhost:8080/"
    ts <- loadTemplates
    run 8080 $ appmm $ app ts m

loadTemplates :: IO Templates
loadTemplates = return (const Nothing)

app :: Templates -> GameStore -> Application
app ts m = queryString >>= (\q ->
    websocketsOr defaultConnectionOptions (wsApp m q) (pageApp ts m))
