module Language.Mimsa.Server.Swagger (outputJSON) where

import qualified Data.Aeson as JSON
import Data.ByteString.Lazy
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Proxy
import Data.Swagger
import Language.Mimsa.Server.Servant
import Servant.Swagger

mimsaSwagger :: Swagger
mimsaSwagger = toSwagger (Proxy :: Proxy MimsaAPI)

outputSwaggerJSON :: ByteString
outputSwaggerJSON = JSON.encode mimsaSwagger

outputJSON :: IO ()
outputJSON = BS.putStrLn outputSwaggerJSON
