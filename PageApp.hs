{-# LANGUAGE OverloadedStrings #-}
module PageApp where
import Network.Wai
import Network.HTTP.Types
import qualified Control.Concurrent.Map as Map
--import Network.HTTP.Types

pageApp :: Map.Map () () -> Application
pageApp m request respond = respond $ responseLBS
        status200
        [("Content-Type", "text/plain")]
        "Hello, Web!"
--respond $ responseLBS status400 [] "Not a WebSocket request"