{-# LANGUAGE OverloadedStrings #-}
module Pages(Handler,GameHandler,
    request,gameStore,
    homePage,newGameh,
    waitPage,checkRequest,
    startGamePage, startGameh) where

import Network.Wai
import Template
import Tools
import Data
import Game(gameURL)--should this be in Data?

import Network.HTTP.Types
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
import System.Random
import Data.Map(fromList)

import qualified Data.Aeson as A --(ToJSON(..),object,Value)

--type Handler =  Request -> GameStore -> IO (Templates -> Response)

--type ($) a b = a b

type HandlerM = ExceptT Response (ReaderT (Request,GameStoreList)
    (StateT (Maybe StdGen)  IO))

type Handler = HandlerM (Templates -> Response)

type GameHandler g = GameInfo -> GameStore g -> HandlerM (Templates -> Response)

request :: HandlerM Request
request = fst<$>ask
askQuery :: HandlerM Query
askQuery = queryString <$>  request

getParam :: C.ByteString -> HandlerM (Maybe C.ByteString)
getParam p = (join . lookup p) <$> askQuery

getParam' :: C.ByteString -> HandlerM C.ByteString
getParam' s = getParam s >>= (? badRequest)

gameStore :: HandlerM GameStoreList
gameStore = snd<$>ask

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
randomChar = ((['a'..'z']++['A'..'Z']++['0'..'9']++['/','+'])!!)
    <$> (`mod`64) <$> randomInt --26+26+10+2=64
    --(chr.(+65).(`mod` 64)) <$> randomInt

randomString :: HandlerM C.ByteString
randomString = C.pack <$> mapM (const randomChar) [1..20]

debug :: String -> Response
debug = responseLBS internalServerError500 [("Content-Type","text/plain")]
    . CL.pack

badRequest :: Response --Note: warp overwrites status code (although not here because it's not a file?)
badRequest = responseLBS badRequest400 [] "400 Bad request"

loadWith :: FilePath -> [(Variable,Value)] -> Templates -> Response
loadWith path env ts = fromBoth $ do
    temp <- ts path ? debug ("template {} not found"%path)
    case runM (eval temp) (EvalConfig ts False) (Map.fromList env) of
        Left err -> Left $ debug err
        Right body -> return $
            responseLBS ok200 [(hContentType,"text/html")] (CL.pack body)


homePage :: Handler
homePage = return $ "games.html" `loadWith` [
    ("gameList", Lst [Lst [Str tag, Str name, Lst []] | GameInfo{tag=tag,name=name} <- games]),
    ("path",Lst [Lst[Str "/" ,Str "home"]])]

newGameh :: Game g => GameHandler g
newGameh info store = do
    playerID <- randomString --getParam "playerID" >>= maybe randomString return
    opp <- getParam "opp" >>= (? badRequest)
    gid <- randomString
    turn <- getParam "turn"
    let (pl0, pl1) =
            (if turn /= Just "false" then id else swap) (playerID,opp)
    md <- liftIO $ newMD pl0 pl1 gid

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

getThings :: [C.ByteString] -> HandlerM [(Variable,Value)]
getThings = mapM (\s -> do
    val <- getParam s >>= (? badRequest)
    return (C.unpack s, Str $ C.unpack val) ) -- This will probably change to use bytestrings

waitPage :: Game g => GameHandler g
waitPage info store = do
    loadWith "wait.html" <$> getThings ["playerID", "gameID"]

checkRequest :: Game g => GameHandler g
checkRequest info store = do
    gmNum <- getParam' "gameID"
    gm <- (liftIO$ CMap.lookup gmNum store) >>= (? responseLBS ok200 [] "no") --no game found? --could be 404, 422
    (game, md@MD{status=s,player0=p0}) <- liftIO$ readMVar gm

    let msg = [("gameID".: gmNum)]
    case s of
        Unstarted  -> do
            --check time
            throwError undefined
        (IsTurn p) -> throwError (jsonResp$A.object$ (msg++[("request".:"goto") , ("target".:gameURL md (tag info))]))
        _          -> do
            liftIO$ putStrLn$ "waiting for a finished game"
    --d <-(gm ? undefined)
    return undefined

startGamePage :: Game g => GameHandler g
startGamePage info store = undefined

startGameh :: Game g => GameHandler g
startGameh info store = undefined

jsonResp :: A.Value -> Response
jsonResp = responseLBS ok200 [("Content-Type","text/html")] . A.encode

--encodeToTextBuilder :: A.Value -> Builder
--encodeToTextBuilder = A.encodeToTextBuilder
