{-# LANGUAGE BangPatterns #-}

module Language.Mimsa.Logging (debugLog, debugPretty, debugPrettyM) where

import qualified Data.Text as T
import Debug.Trace
import Language.Mimsa.Core (Printer (..))

debugLog :: (Show b) => String -> b -> b
debugLog title item = snd (traceShowId (title, item))

debugPretty :: (Printer b) => String -> b -> b
debugPretty title !item =
  let !output = title <> ":\n" <> T.unpack (prettyPrint item)
   in trace output item

debugPrettyM :: (Applicative m, Printer a) => String -> a -> m ()
debugPrettyM title !item =
  traceM (title <> ":\n" <> T.unpack (prettyPrint item))
