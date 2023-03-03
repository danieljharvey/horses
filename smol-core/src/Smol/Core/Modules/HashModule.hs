module Smol.Core.Modules.HashModule (serializeModule, deserializeModule) where

import Data.Bifunctor
import Data.Coerce
import Data.Functor
import Smol.Core
import Smol.Core.Types.Module.Module
import Smol.Core.Types.Module.ModuleHash
import Crypto.Hash (SHA256 (..), hashWith)
import qualified Data.Aeson as JSON
import Data.ByteArray.Encoding (Base (Base16), convertToBase)
import qualified Data.ByteString.Lazy as LBS
import Data.ByteString.Lazy.Char8 (toStrict)
import Data.Text.Encoding

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

contentAndHash :: (JSON.ToJSON a) => a -> (LBS.ByteString, ModuleHash)
contentAndHash a =
  let json' = JSON.encode a
      hash' =
        ModuleHash
          . decodeUtf8
          . convertToBase Base16
          . hashWith SHA256
          . toStrict
          $ json'
   in (json', hash')
