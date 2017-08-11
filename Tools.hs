module Tools(unless,withError) where

import qualified Control.Monad as CM
import Control.Monad.Except hiding (unless)
import qualified Data.Map as Map

infix 0 `unless`
unless :: MonadError e m => e -> Bool -> m ()
unless= flip CM.unless . throwError

withError :: MonadError e m => (e->e) -> m a -> m a
withError f xm = catchError xm (throwError.f)