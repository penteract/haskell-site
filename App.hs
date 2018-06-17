--{-# LANGUAGE OverloadedStrings #-}

import Network.Wai --
import Network.HTTP.Types
import Network.Wai.Handler.Warp (runSettings, setPort, setFdCacheDuration, defaultSettings)
import Network.Wai.Handler.WebSockets
import Network.WebSockets hiding(requestHeaders) --(defaultConnectionOptions,acceptRequest,sendTextData,ServerApp)
--import Data.Text(Text)

import PageApp(pageApp)
import WSApp(wsApp)
import Data (GameStoreList(GSL))
import Template (Templates,loadTemplates)
import Utils(lookIn)

import qualified Control.Concurrent.Map as Map

--import Template

appmm :: Middleware
appmm a1 req respond = do
    putStrLn $ unlines $ map ($req)  [const "",
        ("requestMethod:"++).show.requestMethod,
        --("httpVersion:"++).show.httpVersion,
        ("rawPathInfo:"++).show.rawPathInfo,
        ("rawQueryString:"++).show.rawQueryString,
        --("requestHeaders:"++).show.requestHeaders,
        --("isSecure:"++).show.isSecure,
        --("remoteHost:"++).show.remoteHost,
        ("pathInfo:"++).show.pathInfo,
        ("queryString:"++).show.queryString,
        ("requestBodyLength:"++).show.requestBodyLength]
        --("requestHeaderHost:"++).show.requestHeaderHost,
        --("requestHeaderRange:"++).show.requestHeaderRange,
        --("requestHeaderReferer:"++).show.requestHeaderReferer,
        --("requestHeaderUserAgent:"++).show.requestHeaderUserAgent]
    a1 req (\resp -> putStrLn (show$ responseStatus resp) >> respond resp)
    {-respond $ responseLBS
        status200
        [("Content-Type", "text/plain")]
        "Hello, Web!"-}

main :: IO ()
main = do
    m <- GSL <$> Map.empty
    putStrLn $ "http://localhost:"++show port ++"/"
    ts' <- ((lookIn$) <$>) <$> loadTemplates "html"
    case ts' of
        Right ts -> runSettings settings $ appmm $ app ts m
        Left e -> putStrLn e
    where
        port=8080
        settings = setPort port $ setFdCacheDuration 10 $ defaultSettings

app :: Templates -> GameStoreList -> Application
app ts m = queryString >>= (\q ->
    websocketsOr defaultConnectionOptions (wsApp m q) (pageApp ts m))
