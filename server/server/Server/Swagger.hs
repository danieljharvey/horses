module Server.Swagger (outputJSON) where

import qualified Data.Aeson as JSON
import Data.ByteString.Lazy
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.OpenApi
import Data.Proxy
import Servant.OpenApi
import Server.Servant

mimsaSwagger :: OpenApi
mimsaSwagger = toOpenApi (Proxy :: Proxy MimsaAPI)

outputSwaggerJSON :: ByteString
outputSwaggerJSON = JSON.encode mimsaSwagger

outputJSON :: IO ()
outputJSON = BS.putStrLn outputSwaggerJSON
