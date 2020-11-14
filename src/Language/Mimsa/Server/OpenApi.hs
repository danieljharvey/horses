module Language.Mimsa.Server.OpenApi (outputJSON) where

import qualified Data.Aeson as JSON
import Data.ByteString.Lazy
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.OpenApi
import Data.Proxy
import Language.Mimsa.Server.Type
import Servant.OpenApi

mimsaOpenApi :: OpenApi
mimsaOpenApi = toOpenApi (Proxy :: Proxy MimsaAPI)

outputOpenApiJSON :: ByteString
outputOpenApiJSON = JSON.encode mimsaOpenApi

outputJSON :: IO ()
outputJSON = BS.putStrLn outputOpenApiJSON
