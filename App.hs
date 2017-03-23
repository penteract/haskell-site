{-# LANGUAGE OverloadedStrings #-}
import Network.Wai
import Network.HTTP.Types
import Network.Wai.Handler.Warp (run)

import Template

app :: Application
app req respond = do
    putStrLn $ unlines $ map ($req)  [
        const "I've done some IO here",
        ("requestMethod:"++).show.requestMethod,
        ("httpVersion:"++).show.httpVersion,
        ("rawPathInfo:"++).show.rawPathInfo,
        ("rawQueryString:"++).show.rawQueryString,
        ("requestHeaders:"++).show.requestHeaders,
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
    respond $ responseLBS
        status200
        [("Content-Type", "text/plain")]
        "Hello, Web!"

main :: IO ()
main = do
    putStrLn $ "http://localhost:8080/"
    run 8080 app