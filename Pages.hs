{-# LANGUAGE OverloadedStrings #-}
module Pages(Handler,GameHandler,
    request,gameStore,
    homePage,newGameh,
    waitPage,checkRequest,
    startGamePage, startGameh,
    playPage,
    pageNotFound) where

import Network.Wai
import Template
import Utils
import Data
import Game(gameURL)--should this be in Data?

import Network.HTTP.Types
import System.Random
import Control.Concurrent.MVar
import qualified Control.Concurrent.Map as CMap
import Control.Monad.Reader hiding (unless)
import Control.Monad.State hiding (unless)
import Control.Monad.Except hiding (unless)
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as CL
import qualified Data.Map as Map
import Data.Tuple(swap)
import Data.Char
import Data.Map(fromList)
import Data.Maybe(fromMaybe)
import System.FilePath((</>))
import Data.List(elemIndex)

--import qualified Data.Aeson as A --(ToJSON(..),object,Value)
import Text.JSON

--type Handler =  Request -> GameStore -> IO (Templates -> Response)

--type ($) a b = a b

type HandlerM = ExceptT Response (ReaderT (Request,GameStoreList)
    (StateT (Maybe StdGen)  IO))

type Handler = HandlerM (Templates -> Response)

type GameHandler g = GameInfo -> GameStore g -> HandlerM (Templates -> Response)

returnPage :: Response -> HandlerM a
returnPage = throwError

request :: HandlerM Request
request = fst<$>ask
askQuery :: HandlerM Query
askQuery = queryString <$> request

getParam :: C.ByteString -> HandlerM (Maybe C.ByteString)
getParam p = (join . lookup p) <$> askQuery
getParam' :: C.ByteString -> HandlerM C.ByteString
getParam' s = getParam s >>= (? badRequest)

getBodyParam :: C.ByteString -> HandlerM C.ByteString
getBodyParam p = (join . lookup p . parseQuery) <$> (request >>= liftIO.requestBody) >>= (? badRequest)

gameStore :: HandlerM GameStoreList
gameStore = snd<$>ask

getGame :: Game g => GameID -> GameStore g -> HandlerM (Maybe (MVar (g,MetaData)))
getGame gmNum store = (liftIO$ CMap.lookup gmNum store)
getGame' :: Game g => GameID -> GameStore g -> Response -> HandlerM (MVar (g,MetaData))
getGame' gmNum store resp = (liftIO$ CMap.lookup gmNum store) >>= (? resp)

--getCurrentPlayer :: HM (Maybe C.ByteString)
--getCurrentPlayer = (join . lookup "player") <$> askQuery


--Avoids performing IO actions that require synchronisation if possible
randomInt :: HandlerM Int
randomInt = do
    gen <- maybe (liftIO newStdGen) return =<< get
    let (result,nextRandom) = next gen
    put (Just nextRandom)
    return result

randomChar :: HandlerM Char
randomChar = ((['a'..'z']++['A'..'Z']++['0'..'9']++['-','_'])!!)
    <$> (`mod`64) <$> randomInt --26+26+10+2=64
    --(chr.(+65).(`mod` 64)) <$> randomInt

randomString :: HandlerM C.ByteString
randomString = C.pack <$> mapM (const randomChar) [1..20]

badRequest :: Response
badRequest = responseLBS badRequest400 [] "400 Bad request"

debug :: String -> Response
debug = serverError -- responseLBS internalServerError500 [(hContentType,"text/plain")]. CL.pack


--Loads a template specified by a filename with a set of values
loadWith :: FilePath -> [(Variable,Value)] -> Templates -> Response
loadWith path env ts =
    case getTemplate path ts >>= (\temp -> runM (eval temp) (EvalConfig ts False) (Map.fromList env)) of
        Left err ->  debug (err++path)
        Right body -> responseLBS ok200 [(hContentType,"text/html")] (CL.pack body)


{-loadTemplate :: String -> Environment -> Templates -> Response
loadTemplate name env ts = case getTemplate name ts >>= (flip (flip (runM .eval) (EvalConfig ts False)) env) of
    Left err -> serverError err
    Right body -> responseLBS ok200 [("Content-Type","text/html")] (CL.pack body)-}

getThings :: [C.ByteString] -> HandlerM [(Variable,Value)]
getThings = mapM (\s -> do
    val <- getParam s >>= (? badRequest)
    return (C.unpack s, Str $ C.unpack val) ) -- This will probably change to use bytestrings


--Page Handlers
----------------------------

homePage :: Handler
homePage = return $ "games.html" `loadWith` [
    ("gameList", Lst [Lst [Str tag, Str name, Lst []] | GameInfo{tag=tag,name=name} <- games]),
    ("path",Lst []),
    ("lastpath",Str "home")]

newGameh :: Game g => GameHandler g
newGameh info store = do
    playerID <- randomString --getParam "playerID" >>= maybe randomString return
    opp <- getParam "opp" >>= (? badRequest)
    gid <- randomString
    turn <- getParam "turn"
    let p = toEnum$fromEnum$ turn == Just "false"
    let (pl0, pl1) = (if p == Zero then id else swap) (playerID,opp)
    md <- liftIO $ newMD pl0 pl1 p gid

    (gameState,rdUrl) <- case C.unpack opp of
        "y" -> do
            return ((newGame,md),
                CL.concat ["wait?gameID=",CL.fromStrict gid,"&playerID=",CL.fromStrict playerID])
        "r" -> do
            throwError $ debug "'next to press this button' unimplemented"
            -- <- CMap.lookup "r"
            --return ("x ",concat ["/wait?gameID=",gid,"&playerID=",])
        ('a':' ':diff) -> (flip (,) $ CL.fromStrict (C.concat ["play?gameID=",gid,"&playerID=",playerID])) <$>
            case ais diff of
                Just a ->
                    if pl0==opp then do
                        game <- either (throwError.debug) return $ a newGame
                        return (game,md {status = IsTurn One})
                      else return (newGame,md{status = IsTurn Zero})
                Nothing -> throwError badRequest
        _ -> throwError badRequest

    --sg <- (lift $ lift $ (newMVar $ (newGame,newMD "pl0" "pl1" gid)) :: HM (MVar (g, MetaData)))
    sg <- liftIO $ newMVar gameState
    liftIO $ CMap.insert gid sg store
    return $ const $ responseLBS ok200 [] (CL.concat
        [CL.pack (tag info), "/", rdUrl])
    --return $ const $ debug "unimp"
    where
        gid = "gameID"

--readGameInfo :: Game g => GameStore g -> C.ByteString -> HandlerM g
--readGameInfo store gmNum = do
    --gmMVar <- (liftIO$ CMap.lookup gmNum store) >>= (? pageNotFound)
    --readMVar


waitPage :: Game g => GameHandler g
waitPage info store = do
    gmNum <- getParam' "gameID"
    gm <- (liftIO$ CMap.lookup gmNum store) >>= (? pageNotFound)
    (game, md@MD{status=s,player0=pl0,player1=pl1}) <- liftIO$ readMVar gm
    plID <- getParam' "playerID" -- >>= return. (maybe)
    pl <- if pl0==plID then return Zero else ((pl1==plID??badRequest)>> return One)
    things <- getThings ["playerID", "gameID"]
    case s of
        Unstarted p  -> do
            p == pl??badRequest
            --check time
            host <- reader (requestHeaderHost.fst) >>= (? badRequest)
            return$ loadWith "wait.html" (things ++ [("game",Str$ name info),("url",Str$ C.unpack host++"/"++tag info++"/join?gameID="++ C.unpack gmNum), ("path",Lst [Lst[Str "/", Str "Home"]]), ("lastpath", Str"Wait")])
            --return$ loadWith "wait.html" (things ++ [("game",Str$ name info),("url",Str$ C.unpack host++"/join?gameID="++ C.unpack gmNum)])
            --returnJSON [("request".:."wait")]
        _          -> do
            --liftIO$ putStrLn$ "waiting for a finished game"
            returnPage$ responseLBS temporaryRedirect307 [(hLocation, C.concat ["/play?gameID=",gmNum,"&playerID=",plID])] ""

checkRequest :: Game g => GameHandler g
checkRequest info store = do
    gmNum <- getParam' "gameID"
    gm <- (liftIO$ CMap.lookup gmNum store) >>= (? responseLBS ok200 [] "no") --no game found? --could be 404, 422
    (game, md@MD{status=s,player0=pl0,player1=pl1}) <- liftIO$ readMVar gm
    plID <- getParam' "playerID" -- >>= return. (maybe)
    pl <- if pl0==plID then return Zero else ((pl1==plID??badRequest)>> return One)

    let msg = [("gameID".: C.unpack gmNum)]
    let returnJSON dat = returnPage (jsonResp$toJSObject$ (msg++dat))
    let reply answer = returnJSON [("request".:."reply"), ("answer".:.answer)]
    case s of
        Unstarted p  -> do
            --check time
            returnJSON [("request".:."wait")]
        (IsTurn p) -> reply "yes" --[("request".:."goto") , ("target".:.gameURL md (tag info) (Just pl))]
        _          -> do
            liftIO$ putStrLn$ "waiting for a finished game"
            reply "no"
            --returnPage$ responseLBS ok200 [] "no"


startGamePage :: Game g => GameHandler g
startGamePage info store = do
    loadWith "startGame.html" <$> getThings ["gameID"]

startGameh :: Game g => GameHandler g
startGameh info store = do
    {-r <- request >>= liftIO.requestBody
    gmNum' <- case decodeStrict. C.unpack$ r of
        Ok (r') -> return (valFromObj "gameID" r')
        _ -> returnPage$debug (show r) --returnPage badRequest
    gmNum <- C.pack <$> fromJSString <$> case gmNum' of
        Ok (JSString s) -> return s
        _ -> returnPage$ debug (show r)-}
    --parseQuery
    gmNum <- getBodyParam "gameID"
    gmVar <- getGame' gmNum store pageNotFound --perhaps this should be a different error
    (gm,md@MD{player0=pl0,player1=pl1}) <- liftIO (takeMVar gmVar)
    case status md of
        Unstarted p -> do
            liftIO (putMVar gmVar (gm,md{status=IsTurn Zero}))
            --let pl = if p==Zero then pl0 else pl1
            returnPage (responseLBS ok200  [("Content-Type","text/plain")] (CL.pack (gameURL md (tag info) (Just p))))
        _ -> liftIO (putMVar gmVar (gm,md)) >> returnPage (responseLBS ok200 [("Content-Type","text/plain")] "/")
    --return undefined

playPage :: Game g => GameHandler g
playPage info store = do
    gmNum <- getParam' "gameID"
    plID <- getParam' "playerID" --could change later to allow observers
    --liftIO (putStrLn "hi")
    gmVar <- getGame' gmNum store pageNotFound
    (gm,md@MD{player0=pl0, player1=pl1}) <- liftIO (readMVar gmVar)
    pln <- elemIndex plID [pl0,pl1] ? forbidden
    v <- getParam "view"
    let vs = views info
    let view = fromMaybe (snd$ head vs) (v >>= lookIn vs)
    let tpath = tag info </> C.unpack view
    let values = [("path",Lst [Lst[Str "/", Str "Home"]]),
         ("lastpath", Str"Wait"),
         ("links",Lst[Lst[Str$ "?pageType={}&gameID={}&playerID={}"%C.unpack ptype%C.unpack gmNum%C.unpack plID,
                          Str$ C.unpack ptype++" view"] | (ptype,file) <- vs,  ptype/=view])
                            ]++
           map (\(x,y)->(x,Str y))
            [{-("player","You"),-} ("playerID", C.unpack plID), ("playern", show pln),
            ("player"++show pln,"You"),("player"++show(1-pln),"Opponent"),
            ("gameID", C.unpack gmNum),
             ("data" , encode (getData gm))]
    return$ tpath `loadWith` values


makeMoveh :: Game g => GameHandler g
makeMoveh info store = do
    gmNum <- getParam' "gameID"
    plID <- getParam' "playerID"
    pos <- getParam "pos"
    gmVar <- getGame' gmNum store pageNotFound
    (gm,md@MD{player0=pl0, player1=pl1}) <- liftIO (readMVar gmVar)
    pln <- elemIndex plID [pl0,pl1] ? forbidden
    returnPage$ debug "unimplemented"
{-
    let vs = views info
    let view = fromMaybe (snd$ head vs) (v >>= lookIn vs)
    let tpath = tag info </> C.unpack view
    let values = [("path",Lst [Lst[Str "/", Str "Home"]]),
         ("lastpath", Str"Wait"),
         ("links",Lst[Lst[Str$ "?pageType={}&gameID={}&playerID={}"%C.unpack ptype%C.unpack gmNum%C.unpack plID,
                          Str$ C.unpack ptype++" view"] | (ptype,file) <- vs,  ptype/=view])
                            ]++
           map (\(x,y)->(x,Str y))
            [{-("player","You"),-} ("playerID", C.unpack plID), ("playern", show pln),
            ("player"++show pln,"You"),("player"++show(1-pln),"Opponent"),
            ("gameID", C.unpack gmNum),
             ("data" , encode (getData gm))]
    return$ tpath `loadWith` values-}


--Status pages
-----------------------------------

pageNotFound :: Response --Note: warp overwrites status code (hence not using file)
pageNotFound = responseLBS notFound404 [(hContentType,"text/html")]
    (CL.unlines ["<html>",
    " <head>",
    "  <title>404 Not Found</title>",
    " </head>",
    " <body>",
    "  <h1>404 Not Found</h1>",
    "  The resource could not be found.<br /><br />",
    "",
    "",
    "",
    " </body>",
    " </html>"])

forbidden :: Response --Note: warp overwrites status code (hence not using file)
forbidden = responseLBS forbidden403 [(hContentType,"text/html")]
    (CL.unlines ["<html>",
    " <head>",
    "  <title>403 Forbidden</title>",
    " </head>",
    " <body>",
    "  <h1>403 Forbidden</h1>",
    "  You do not have permission to access this resource<br /><br />",
    " </body>",
    " </html>"])


serverError :: String -> Response
serverError err = responseLBS (Status 500 (C.pack err)) [(hContentType,"text/html")]
        (CL.unlines ["<html>",
        " <head>",
        "  <title>500 Internal Server Error</title>",
        " </head>",
        " <body>",
        "  <h1>500 Internal Server Error</h1>",
        "  Something went wrong and it's my fault<br /><br />",
        CL.pack err,
        "",
        "",
        " </body>",
        " </html>"])

jsonResp :: JSObject JSValue -> Response
jsonResp = responseLBS ok200 [(hContentType,"application/json")] . CL.pack . ($"") .showJSObject

--encodeToTextBuilder :: A.Value -> Builder
--encodeToTextBuilder = A.encodeToTextBuilder
