--module Prep where

import System.Environment
--import System.Directory
--import System.FilePath
--import Data.Char
--import Data.Maybe(fromMaybe)

import Template
import Data

--type FileContents = String

{-
-- helper function for making strings nicely
(%) :: String -> String -> String
(%) ('{':'}':s) x = x++s
(%) ('\\':c:s) xs = c:(s%xs)
(%) (c:s) xs = c:(s%xs)
(%) "" _ = error "not enough '{}' in string"
-}

main :: IO ()
main = do
   [inDir,outDir] <- getArgs
   procDir [("four",Lst $ map (Str .show) [0..3]),
       ("gameList", Lst [Lst [Str tag, Str name, Lst []] | (_,GameInfo{tag=tag,name=name}) <- games])] inDir outDir
