module Smol.Core.Modules.HashModule (serializeModule, deserializeModule) where

import Crypto.Hash (SHA256 (..), hashWith)
import qualified Data.Aeson as JSON
import Data.Bifunctor
import Data.ByteArray.Encoding (Base (Base16), convertToBase)
import qualified Data.ByteString.Lazy as LBS
import Data.ByteString.Lazy.Char8 (toStrict)
import Data.Coerce
import Data.Functor
import Data.Text.Encoding
import Smol.Core.Types.Constructor
import Smol.Core.Types.Identifier
import Smol.Core.Types.Module.Module
import Smol.Core.Types.Module.ModuleHash
import Smol.Core.Types.TypeName

-- we remove annotations before producing the hash
-- so formatting does not affect it
hashModule ::
  ( JSON.ToJSON (dep TypeName),
    JSON.ToJSON (dep Constructor),
    JSON.ToJSON (dep Identifier),
    JSON.ToJSONKey (dep Identifier)
  ) =>
  Module dep ann ->
  (LBS.ByteString, ModuleHash)
hashModule mod' = second coerce . contentAndHash $ mod' $> ()

-- this is the only encode we should be doing
serializeModule ::
  ( JSON.ToJSON (dep TypeName),
    JSON.ToJSON (dep Constructor),
    JSON.ToJSON (dep Identifier),
    JSON.ToJSONKey (dep Identifier)
  ) =>
  Module dep ann ->
  (LBS.ByteString, ModuleHash)
serializeModule = hashModule

-- this is the only json decode we should be doing
deserializeModule ::
  ( Ord (dep Identifier),
    JSON.FromJSONKey (dep Identifier),
    JSON.FromJSON (dep TypeName),
    JSON.FromJSON (dep Constructor),
    JSON.FromJSON (dep Identifier)
  ) =>
  LBS.ByteString ->
  Maybe (Module dep ())
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
