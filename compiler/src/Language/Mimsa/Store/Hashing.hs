module Language.Mimsa.Store.Hashing
  ( contentAndHash,
  )
where

import Crypto.Hash (SHA256 (..), hashWith)
import qualified Data.Aeson as JSON
import Data.ByteArray.Encoding (Base (Base16), convertToBase)
import qualified Data.ByteString.Lazy as LBS
import Data.ByteString.Lazy.Char8 (toStrict)
import Data.Text.Encoding
import Language.Mimsa.Types.Project

contentAndHash :: (JSON.ToJSON a) => a -> (LBS.ByteString, ProjectHash)
contentAndHash a =
  let json' = JSON.encode a
      hash' =
        ProjectHash . decodeUtf8
          . convertToBase Base16
          . hashWith SHA256
          . toStrict
          $ json'
   in (json', hash')
