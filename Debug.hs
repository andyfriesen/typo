module Debug
    ( trace
    , traceM
    , traceMShow
    ) where

import Debug.Trace

traceM :: Monad m => String -> m ()
traceM s = trace s (return ())

traceMShow :: (Monad m, Show s) => s -> m ()
traceMShow = traceM (show s)
