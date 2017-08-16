module Template(procDir,Value(..),Variable,
    Template,Templates,runM,EvalConfig(..),eval,loadTemplates) where


--import System.Environment
import System.Directory
import System.FilePath
import Data.Char
import Data.List
import Data.Maybe(fromMaybe)
import Tools--(unless,(?+),(??),(%))

import qualified Control.Monad as CM
import Control.Monad.State hiding (unless)
import Control.Monad.RWS hiding (unless)
import Control.Monad.Except hiding (unless)
import qualified Data.Map as Map

--this took too much work
{-
ifErr :: MonadError e m => m a -> e -> m a
ifErr xm e = withError (const e) xm-}

type FileContents = String
type Templates = FilePath -> Maybe Template

procDir :: [(Variable,Value)]->FilePath->FilePath-> IO ()
procDir env inDir outDir= do --IO monad
    allFiles inDir >>= putStrLn.show
    buildDir inDir outDir
    parsed <- loadTemplates inDir
    let err = do
            fps <- parsed
            let config=EvalConfig{files = flip lookup fps,isPrep=True}
            sequence [(,) fn <$> (runM (eval tmp) config (Map.fromList env) ?+
              ("While building "++fn++"\n" ))
                 | (fn,tmp) <- fps, (snd $ head tmp) /= Template]
    {-
    err <- loadTemplates inDir fnames >>=  return . (=<<) (\fps ->
        let config=EvalConfig{files = flip lookup fps,isPrep=True} in
        sequence [(,) fn <$> (runM (eval tmp) config (Map.fromList env) ?+
          ("While building "++fn++"\n" ))
             | (fn,tmp) <- fps, (snd $ head tmp) /= Template])-}

    case err of
        Left e -> putStrLn "Some Error:" >> putStrLn e
        Right fbs -> mapM_ (uncurry $ writeFile.(outDir</>)) fbs

loadTemplates :: FilePath -> IO (Either String [(FilePath,Template)])
loadTemplates baseDir = allFiles baseDir >>= (\paths -> (\bodies ->
    (zip paths bodies `forM` (\(n,b)->
        (,) n <$> run parse b ?+ "While parsing "++n++"\n")))
    <$> mapM (readFile . (baseDir</>)) paths)
{-
    do -- IO monad
    bodies <- mapM readFile paths

    return $ do -- Either String monad
        parsed <- mapM (\(n,b)-> run parse b ?+ "While parsing "++n++"\n"  ) $
            zip paths bodies
        return $ flip lookup $ zip paths parsed-}


--Given 2 directories, creates subdirectories in the second to match the structure of the first
buildDir :: FilePath -> FilePath -> IO ()
buildDir inD outD = do
    exists <- doesDirectoryExist outD
    if exists then return () else createDirectory outD

    contents <- getDirectoryContents inD

    mapM_ (\ d -> do
      isDir <- doesDirectoryExist (inD</>d)
      if isDir && head d /= '.' then buildDir (inD</>d) (outD</>d) else return ()
      ) contents

--List all files recursively from a given directory
--returns the fragments of paths after the given directory
--ignores things starting with '.'
allFiles :: FilePath -> IO [FilePath]
allFiles p = do
  contents <- filter ((/='.').head) `fmap` getDirectoryContents p
  concat <$> mapM (\ d -> do
      isDir <- doesDirectoryExist (p</>d)
      if isDir then map (d </>) <$> allFiles (p</>d) else return [d]
    ) contents



type Template = [(String,Tag)]

type Variable = String
data Tag = Template | Print Variable | Load FilePath | Set Variable Template
    | Fors [Variable] Variable Template | For Variable Variable Template | End deriving Eq

instance Show Tag where
  show Template = "<<TEMPLATE>>"
  show (Print x) = "<<{}>>"%x
  show (Load fname) = "<<LOAD {}>>"%fname
  show (Set x t) = "<<SET {}= {}>>"%x%showT t
  show (For x y t) = "<<FOR {} IN {}: {}>>"%x%y%showT t
  show (Fors xs y t) = "<<FOR {} IN {}: {}>>"%(intercalate ", " xs)%y%showT t
  show End = ""

showT :: Template->String
showT [] = ""
showT ((s,t):xs) = s++show t++showT xs


--Interpreter
--------------

data Value = Str String | Lst [Value]
type Environment = Map.Map Variable Value
{-
class Valueable a where
    toValue :: a->Value

instance {-# OVERLAPPING  #-} Valueable ([] Char) where
    toValue = Str

instance Valueable a => Valueable [a] where
    toValue = Lst . map toValue

instance Valueable Value where
    toValue = id-}

lookupType :: String -> String -> (String -> Maybe b) -> Either String b
lookupType typ x lookup =
    fromMaybe (Left ("{} '{}' not found"%typ%x)) (Right <$> lookup x)

getVal :: Variable -> Environment -> Either String Value
getVal v env = fromMaybe (Left ("Variable '{}' not found"%v)) (Right <$> Map.lookup v env)

setV :: Variable -> Value -> M ()
setV var val = modify (Map.insert var val)

printV :: Value -> Either String String
printV (Str s) = Right s
printV _ = Left "Value is not a string"

getTemplate :: FilePath -> Templates -> Either String Template
getTemplate = lookupType "Template"

data EvalConfig = EvalConfig{
    files :: Templates,
    isPrep :: Bool
    }

type M = RWST EvalConfig String Environment (Either String)

runM :: M a -> EvalConfig -> Environment -> Either String String
runM m c env = snd <$> evalRWST m c env

--A rearrangement of the arguments of runM
--runIn :: [(FilePath,Template)] -> Environment -> M a -> Either String (a,String)
--runIn fs env m = runM m fs env

eval :: Template -> M ()
eval [] = throwError "Badly terminated template"
eval [(s,End)] = tell s
eval ((s,tag):rest) = do
    tell s
    evalT tag
    eval rest


withVal :: Variable -> String -> (Value -> M ()) -> M ()
withVal var name f = do
    env <- get
    isp <- asks isPrep
    either (if isp then const $ tell name else lift . Left) f (getVal var env)


evalT :: Tag -> M ()
evalT Template =  return ()
--evalT tag@(Print x) = withVal x (show tag) ((tell=<<).lift.printV)
evalT tag@(Print x) = fromMaybe (Str "") <$> (gets (Map.lookup x)) >>= ((tell=<<).lift.printV)
--     env <- get
--     isp <- asks isPrep
--     either (if isp then const $ tell $ show $ Print x else lift . Left . id) ((tell=<<).lift.printV) (getVal x env)
    --str <- lift $ getVal x env >>= printV
    --tell str
evalT (Load fname) = asks files >>= (lift . getTemplate fname) >>= eval
    ?+ "in {}\n"%fname
evalT (Set x t) = do
    val <- censor (const "") $ fmap snd . listen $ eval t
    setV x (Str val)
evalT tag@(For xName yName t) = do
    y <- fromMaybe (Lst []) <$> (gets (Map.lookup yName))
    yl <- (case y of
        Lst l -> return l
        _ -> throwError ("trying to iterate over {} which is not a list"%yName))
    evalFor yName t [xName] [Lst [x] | x <- yl]
evalT tag@(Fors vars yName t) = do
    y <- fromMaybe (Lst []) <$> (gets (Map.lookup yName))
    yl <- (case y of
        Lst l -> return l
        _ -> throwError ("trying to iterate over {} which is not a list"%yName))
    evalFor yName t vars yl
evalT End = return ()

evalFor :: String -> Template -> [Variable] -> [Value] -> M ()
evalFor yName tmp vars vals = mapM_ (\xs-> do
        l <- case xs of
          Lst l -> return l
          Str _ -> throwError ("{} expected to be List of Lists"%yName)
        ("elements of {} do not have length {}"%yName%show(length vars))
             `unless` length vars == length l
        _ <- zipWithM setV vars l
        eval tmp) vals


-- Parser
----------

--I don't think it's easier to create a monad transformer
type Parser = StateT FileContents (Either String)

run :: Parser a -> FileContents -> Either String a
run p = evalStateT (p <* (get >>= (\cont ->
  ("Did not consume all input. The remainder begins with" ++ take 20 cont) `unless` (cont == ""))))
--run p cont = (\(tmp,out)-> if )  <$> runStateT p cont

--There is probably a better way of doing this
--(one way would be to modify the single usage)
unRun :: (FileContents->Either String (a,FileContents))-> Parser a
unRun p = get>>=(either throwError (\(a,r)->put r>>return a)).p

parse :: Parser Template
parse = get >>= parse'

parse' :: FileContents -> Parser Template
parse' "" = return [("",End)]
parse' ('>':'>':_) = return [("",End)]
parse' ('<':'<':s) = do
    put s
    t <- parseTag
    tmp    <- parse
    return (("",t):tmp)
parse' (c:rs) = do
    put rs
    ((s,t):tmp) <- parse
    return ((c:s,t):tmp)

parseTag :: Parser Tag
parseTag = do
     tok <- getMatch (all isAlphaNum)
     (case tok of
          "TEMPLATE" ->return Template
          "SET" -> parseSet
          "FOR" -> parseFor
          "LOAD" -> parseLoad
          "" ->  get >>= throwError.take 20
          s -> (isLower $ head s)??"unrecognised tag" >> return (Print s)
       <* getStr ">>")


parseLoad :: Parser Tag
parseLoad = do
    fname <- getMatch $ all ((||).(=='.') <*> isAlphaNum)
    return $ Load fname

parseSet :: Parser Tag
parseSet = do
    vname <- getVar
    getStr "="
    tmp <- parse
    return $ Set vname tmp

parseFor :: Parser Tag
parseFor = do
    parseFor1 ||| parseFors

parseFor1 :: Parser Tag
parseFor1 = do
    xName <- getVar
    getStr "IN"
    xsName <- getVar
    getStr ":"
    tmp <- parse
    return $ For xName xsName tmp

parseFors :: Parser Tag
parseFors = do
    xName <- getVar
    (do
        getStr ","
        (Fors xNames xsName tmp) <- parseFors
        return $ Fors (xName:xNames) xsName tmp
        ) ||| (do
        getStr "IN"
        xsName <- getVar
        getStr ":"
        tmp <- parse
        return $ Fors [xName] xsName tmp)


strip :: String -> String
strip = dropWhile isSpace


(|||) :: Parser a -> Parser a -> Parser a
p1 ||| p2 = do
    s <- get
    catchError p1 (\e-> put s >>  p2 ?+ e++" neither \n")

--finds the longest match, stripping whitespace
--checks for nonempty matches and finds the last
getMatch :: (String->Bool) -> Parser String
getMatch cond = unRun (\inp->
    let
        stripped = strip inp
        tokens = takeWhile cond $ tail $inits stripped
        last' = foldl (const Right) (Left ("No matching token from:"++take 10 inp))
        --last' = foldl (\x (y,c)->if y/= "" then Right (y,c) else Left ("****"++inp)) (Left ("No matching token from:"++take 10 inp))
        in
    last' . zip tokens . map strip $ tail $tails stripped)

getVar :: Parser Variable
getVar = getMatch (((&&).isLower.head) <*> all isAlphaNum)

getStr :: String -> Parser ()
getStr s = do
    m <- getMatch (`isPrefixOf` s) ?+ "{} expected"%s
    m==s ?? "{} expected"%s
