{-# LANGUAGE OverloadedStrings #-}
module PageApp where

import Data(GameStoreList,GameStore,Game,GameInfo(..), Retriveable(..),games)
import Template
import Utils
import Pages

import Data.List
import Data.Char(toLower)
import Data.Maybe(fromMaybe)
import Network.Wai
--Network.HTTP.Types does not currently export status308
import Network.HTTP.Types.Status
import Network.HTTP.Types.Method
import Network.HTTP.Types.Header
import qualified Data.Map as Map
import qualified Control.Concurrent.Map as CMap
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as CL
--import Control.Monad.Reader(runReaderT,join,lift)
--import Control.Monad.State(evalStateT)
import Control.Monad.Except(runExceptT)
import Control.Monad.RWS(evalRWST,join,lift)
import System.FilePath((</>))
import System.Random

type Page = [(Method,Handler)]

rdrs = [("/favicon.ico","/staticfiles/favicon.ico"),("/3sphere","/staticfiles/3-sphere.html")]

--cannonise :: String -> String
cannonise = C.map toLower

pageList :: [(C.ByteString, [(Method,Handler)])]
pageList =  ([("/",[(methodGet,homePage)])] ++
    concat [procPages info getStore | (Retrive getStore, info)<-games ])



globalPages :: [(C.ByteString, [(Method,Handler)])]
globalPages = [
    ("/",[(methodGet,homePage)])]

procPages :: Game g => GameInfo -> (GameStoreList -> GameStore g) ->
    [(C.ByteString, [(Method, Handler)])]
procPages info getS = [(C.concat["/",C.pack$ tag info,"/",path],
    [(meth,(getS<$> gameStore) >>= h info)
        | (meth,h)<-hs])
    | (path,hs) <- perGamePages]

perGamePages :: Game g => [(C.ByteString, [(Method, GameHandler g)])]
perGamePages = [
    ("new",[(methodPost,newGameh)]),
    ("wait",[(methodGet,waitPage)]),
    ("checkrequest", [(methodGet, checkRequest)]),
    ("join", [(methodGet, startGamePage)]),
    ("startgamepost", [(methodPost, startGameh)]),
    ("play", [(methodGet,playPage)]),
    ("makemove", [(methodPost, makeMoveh)])
    ]

--[([Char],GameHandler g)]
--perGame (path,page) =
--    [(tag </> path, )| game <- games]

--function for rearranging arguments while presenting a wai-style interface
pageApp :: Templates -> GameStoreList -> Application
pageApp ts gs req resp =
    evalRWST (runExceptT (pageApp' (lookIn pageList))) (req,gs) Nothing >>= ret
    where
        ret (Left p, w)  = w >> resp p
        ret (Right h, w) = resp (h ts)


pageApp' :: (C.ByteString -> Maybe Page) -> Handler
pageApp' getPage = do --HandlerM
    req <- request
    let path = rawPathInfo req
        cannonicalPath = cannonise path
        spath = fromMaybe (C.unpack path) (lookup path rdrs)
        method = requestMethod req
    path == cannonicalPath ?? redirect cannonicalPath
    not ("/staticfiles" `isPrefixOf` spath) ??
        case method of "GET" -> staticFile (tail spath)
                       _     -> allow [methodGet]
    methodHandlers <- getPage path ? pageNotFound
    join $ lookup (requestMethod req) methodHandlers ? allow (map fst methodHandlers)

staticFile :: String -> Response
staticFile path = responseFile ok200 [] path Nothing


redirect :: C.ByteString -> Response
redirect url = responseLBS permanentRedirect308 [(hLocation,url)] ""

allow :: [Method] -> Response --Note: warp overwrites status code
allow methods = responseFile methodNotAllowed405
    [(hAllow, C.intercalate ", " methods),("Content-Type","text/html")]
    ("staticfiles" </> "405.html") Nothing
