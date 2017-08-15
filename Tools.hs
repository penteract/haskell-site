{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}

module Tools where

import Prelude hiding(lookup,insert,delete)
import qualified Prelude
import qualified Control.Monad as CM
import Control.Monad.Except hiding (unless)
import qualified Data.Map.Lazy as LMap
--import qualified Data.Map.Strict as SMap LMap.Map == SMap.Map, preventing typeclass fun

type Lookup k a = k -> Maybe a

-- helper function for making strings nicely
(%) :: String -> String -> String
(%) ('{':'}':s) x = x++s
(%) ('\\':c:s) xs = c:(s%xs)
(%) (c:s) xs = c:(s%xs)
(%) "" _ = error "not enough '{}' in string"

infix 0 `unless`
unless :: MonadError e m => e -> Bool -> m ()
unless= flip CM.unless . throwError

withError :: MonadError e m => (e->e) -> m a -> m a
withError f xm = catchError xm (throwError.f)

-- Prepend some context to an error message
(?+):: Monoid e => MonadError e m => m a -> e -> m a
(?+) = flip $ withError.mappend

infixl 1 ?+

assert :: MonadError e m => Bool -> e -> m ()
assert = flip unless

(??) :: MonadError e m => Bool -> e -> m ()
(??) = assert

infixl 1 ??

-- Turn a maybe into an error message
(?) :: Maybe a -> b -> Either b a
(?) a b =
    case a of
    Nothing -> Left b
    Just c -> Right c
-- Generated by Djinn

fromBoth :: Either a a -> a
fromBoth a =
           case a of
           Left b -> b
           Right c -> c
-- Generated by Djinn

-- a generic typeclass for maps (lets me avoid typing Map.lookup)
-- and lets me keep functions general
-- this is too much work (but fun)
class Mapping k a m | m -> k a where
    lookup :: Ord k => k -> m -> Maybe a
    insert :: Ord k =>  k -> a -> m -> m
    delete :: Ord k =>  k -> m -> m

instance Mapping k a  (LMap.Map k a) where
    lookup = LMap.lookup
    insert = LMap.insert
    delete = LMap.delete

instance Mapping k a [(k,a)] where
    lookup = Prelude.lookup
    insert = curry (:)
    delete x = filter $ (/=x) . fst

lookIn :: Mapping k a m => Ord k => m -> k -> Maybe a
lookIn = flip lookup
