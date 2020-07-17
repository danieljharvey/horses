module Language.Mimsa.Logging where

import Debug.Trace

debugLog :: (Show b) => String -> b -> b
debugLog title item = snd (traceShowId (title, item))
