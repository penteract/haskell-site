{-# LANGUAGE OverloadedStrings #-}
module Pages(Handler,GameHandler,
    request,gameStore,
    homePage,newGameh) where

import Network.Wai
import Template
import Tools
import Data

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

gameStore :: HandlerM GameStoreList
gameStore = snd<$>ask

--getCurrentPlayer :: HM (Maybe C.ByteString)
--getCurrentPlayer = (join . lookup "player") <$> askQuery


--Avoids performing IO actions that require synchronisation if possible
randomInt :: HandlerM Int
randomInt = do
    gen <- maybe (lift' newStdGen) return =<< get
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

badRequest :: Response --Note: warp overwrites status code
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

lift' = lift.lift.lift

newGameh :: Game g => GameHandler g
newGameh info store = do
    playerID <- randomString --getParam "playerID" >>= maybe randomString return
    opp <-getParam "opp" >>= (? badRequest)
    gid <- randomString
    turn <- getParam "turn"
    let (pl0, pl1) =
            (if turn /= Just "false" then id else swap) (playerID,opp)
    md <- lift' $ newMD pl0 pl1 gid

    (gameState,rdUrl) <- case C.unpack opp of
        "y" -> do
            return ((newGame,md),
                C.concat ["/wait?gameID=",gid,"&playerID=",playerID])
        "r" -> do
            throwError $ debug "'next to press this button' unimplemented"
            -- <- CMap.lookup "r"
            --return ("x ",concat ["/wait?gameID=",gid,"&playerID=",])
        ('a':' ':diff) -> (flip (,) $ C.concat ["/play?gameID=",gid,"&playerID=",playerID]) <$>
            case ais diff of
                Just a ->
                    if pl0==opp then do
                        game <- either (throwError.debug) return $ a newGame
                        return (game,md {status = IsTurn One})
                      else return (newGame,md{status = IsTurn Zero})
                Nothing -> throwError badRequest
        _ -> throwError badRequest

    --sg <- (lift $ lift $ (newMVar $ (newGame,newMD "pl0" "pl1" gid)) :: HM (MVar (g, MetaData)))
    sg <- lift' $ newMVar gameState
    lift' $ CMap.insert gid sg store
    return $ const $ debug "unimp"
    where
        gid = "gameID"
