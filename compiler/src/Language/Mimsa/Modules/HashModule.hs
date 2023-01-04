module Language.Mimsa.Modules.HashModule (serializeModule, deserializeModule) where

import qualified Data.Aeson as JSON
import Data.Bifunctor
import qualified Data.ByteString.Lazy as LBS
import Data.Coerce
import Data.Functor
import Language.Mimsa.Core
import Language.Mimsa.Store.Hashing
import Language.Mimsa.Types.Project.ProjectHash

-- we remove annotations before producing the hash
-- so formatting does not affect it
hashModule :: Module ann -> (LBS.ByteString, ModuleHash)
hashModule mod' = second coerce . contentAndHash $ mod' $> ()

-- this is the only encode we should be doing
serializeModule :: Module ann -> (LBS.ByteString, ModuleHash)
serializeModule = hashModule

-- this is the only json decode we should be doing
deserializeModule :: LBS.ByteString -> Maybe (Module ())
deserializeModule =
  JSON.decode
