{-# LANGUAGE OverloadedStrings #-}

import Network.Wai hiding(requestHeaders)
import Network.HTTP.Types
import Network.Wai.Handler.Warp (run)
import Network.Wai.Handler.WebSockets
import Network.WebSockets--(defaultConnectionOptions,acceptRequest,sendTextData,ServerApp)
--import Data.Text(Text)
import PageApp(pageApp)
import WSApp(wsApp)
import qualified Control.Concurrent.Map as Map

--import Template

appmm :: Middleware
appmm a1 req respond = do
    putStrLn $ unlines $ map ($req)  [
        const "I've done some IO here",
        ("requestMethod:"++).show.requestMethod,
        ("httpVersion:"++).show.httpVersion,
        ("rawPathInfo:"++).show.rawPathInfo,
        ("rawQueryString:"++).show.rawQueryString,
        --("requestHeaders:"++).show.requestHeaders,
        ("isSecure:"++).show.isSecure,
        ("remoteHost:"++).show.remoteHost,
        ("pathInfo:"++).show.pathInfo,
        ("queryString:"++).show.queryString,
        --("requestBody:"++).show.requestBody,
        --("vault:"++).show.vault,
        ("requestBodyLength:"++).show.requestBodyLength,
        ("requestHeaderHost:"++).show.requestHeaderHost,
        ("requestHeaderRange:"++).show.requestHeaderRange,
        ("requestHeaderReferer:"++).show.requestHeaderReferer,
        ("requestHeaderUserAgent:"++).show.requestHeaderUserAgent]
    a1 req respond
    respond $ responseLBS
        status200
        [("Content-Type", "text/plain")]
        "Hello, Web!"

withQuery ::(Query -> Application ) -> Application
withQuery app = (queryString>>=)app

main :: IO ()
main = do
    m <- Map.empty
    putStrLn $ "http://localhost:8080/"
    run 8080 $ appmm $ (app m =<< queryString)

app :: Map.Map () () -> Query -> Application
app m q = websocketsOr defaultConnectionOptions (wsApp m q) (pageApp m)
    

