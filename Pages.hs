{-# LANGUAGE OverloadedStrings #-}
module Pages(Handler,homePage) where

import Network.Wai
import Template
import Tools
import Data

import Network.HTTP.Types.Status
import Network.HTTP.Types.Header
import Control.Concurrent.MVar
import qualified Control.Concurrent.Map as CMap
--import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as CL
import qualified Data.Map as Map


type Handler =  Request -> GameStore -> IO (Templates -> Response)
type GameHandler = GameInfo -> Request -> GameStore -> IO (Templates -> Response)

debug :: String -> Response
debug = responseLBS internalServerError500 [("Content-Type","text/plain")]
    . CL.pack

loadWith :: FilePath -> [(Variable,Value)] -> Templates -> Response
loadWith path env ts = fromBoth $ do
    temp <- ts path ? debug ("template {} not found"%path)
    case runM (eval temp) (EvalConfig ts False) (Map.fromList env) of
        Left err -> Left $ debug err
        Right body -> return $
            responseLBS ok200 [(hContentType,"text/html")] (CL.pack body)


homePage :: Handler
homePage _ _ = return $ "games.html" `loadWith` [
    ("gameList", Lst [Lst [Str tag, Str name, Lst []] | GameInfo{tag=tag,name=name} <- games]),
    ("path",Lst [Lst[Str "/" ,Str "home"]])]

newGamePage :: GameHandler
newGamePage g req gs = do
    sg <- (newMVar $ newGame g "pl0" "pl1" gid)
    CMap.insert gid sg gs
    return $ const $ debug "unimp"
    where
        gid = "gameID"
