{-# LANGUAGE OverloadedStrings #-}
module PageApp where

import Data(GameStoreList(..),GameStore,Game,ox)
import Template
import Tools
import Pages

import Data.List
import Data.Char(toLower)
import Network.Wai
--Network.HTTP.Types does not currently export status308
import Network.HTTP.Types.Status
import Network.HTTP.Types.Method
import Network.HTTP.Types.Header
import qualified Data.Map as Map
import qualified Control.Concurrent.Map as CMap
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as CL
import Control.Monad.Reader(runReaderT,join,lift)
import Control.Monad.State(evalStateT)
import Control.Monad.Except(runExceptT)
import System.FilePath((</>))
import System.Random

type Page = [(Method,Handler)]

--cannonise :: String -> String
cannonise = C.map toLower

--pageList :: C.ByteString -> Maybe Page
pageList =  ([("/",[(methodGet,homePage)])] ++
    concat [procPages getOXStore "ox3" ])



globalPages :: [(C.ByteString, [(Method,Handler)])]
globalPages = [
    ("/",[(methodGet,homePage)])]

procPages :: Game g => (GameStoreList -> GameStore g) -> C.ByteString ->
    [(C.ByteString, [(Method, Handler)])]
procPages getS tag = [(C.concat["/",tag,"/",path],
    [(meth,(getS<$> gameStore) >>= h ox)
        | (meth,h)<-hs])
    | (path,hs) <- perGamePages]

perGamePages :: Game g => [(C.ByteString, [(Method, GameHandler g)])]
perGamePages = [
    ("new",[(methodPost,newGameh)]),
    ("wait",[(methodGet,waitPage)])
    ]

--[([Char],GameHandler g)]
--perGame (path,page) =
--    [(tag </> path, )| game <- games]

--function for rearranging arguments while presenting a wai-style interface
pageApp :: Templates -> GameStoreList -> Application
pageApp ts gs req resp =
    evalStateT (runReaderT (runExceptT (pageApp' (lookIn pageList))) (req,gs)) Nothing >>= ret
    where
        ret (Left p)  = resp p
        ret (Right h) = resp (h ts)

--Consider reordering arguments
pageApp' :: (C.ByteString -> Maybe Page) -> Handler
pageApp' getPage = do --HandlerM
    --lift$lift$lift (putStrLn$unlines (map (C.unpack.fst) pageList))
    req <- request
    let path = rawPathInfo req
        spath = C.unpack path
        cannonicalPath = cannonise path
        method = requestMethod req
  --either (return.return) (id) $ do --Either Resp monad
    path == cannonicalPath ?? redirect cannonicalPath
    not ("/staticfiles" `isPrefixOf` spath && method==methodGet) ??
        staticFile (tail spath)
    methodHandlers <- getPage path ? pageNotFound
    join $ lookup (requestMethod req) methodHandlers ? allow (map fst methodHandlers)

staticFile :: String -> Response
staticFile path = responseFile status200 [] path Nothing

pageNotFound :: Response --Note: warp overwrites status code
pageNotFound = responseFile notFound404 [("Content-Type","text/html")]
    ("staticfiles" </> "404.html") Nothing

redirect :: C.ByteString -> Response
redirect url = responseLBS permanentRedirect308 [(hLocation,url)] ""

allow :: [Method] -> Response --Note: warp overwrites status code
allow methods = responseFile methodNotAllowed405
    [(hAllow, C.intercalate ", " methods),("Content-Type","text/html")]
    ("staticfiles" </> "405.html") Nothing
