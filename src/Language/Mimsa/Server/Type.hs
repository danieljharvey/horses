{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Language.Mimsa.Server.Type
  ( server,
  )
where

import Control.Monad.Except
import Data.Proxy
import qualified Data.Text.IO as T
import Data.Text.Lazy (fromStrict)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Language.Mimsa.Printer
import Language.Mimsa.Store.Storage (findExpr, saveExpr)
import Language.Mimsa.Types.Store
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

type MimsaAPI =
  "store" :> ("expression" :> Capture "exprHash" ExprHash :> Get '[JSON] (StoreExpression ()))
    :<|> ("expression" :> ReqBody '[JSON] (StoreExpression ()) :> Post '[JSON] ExprHash)

mimsaAPI :: Proxy MimsaAPI
mimsaAPI = Proxy

mimsaServer :: Server MimsaAPI
mimsaServer = getExpression :<|> postExpression

getExpression ::
  ExprHash ->
  Handler (StoreExpression ())
getExpression exprHash' =
  Handler $ withExceptT to500Error (findExpr [] exprHash')

postExpression ::
  StoreExpression () ->
  Handler ExprHash
postExpression se =
  Handler $ withExceptT to500Error (saveExpr se)

to500Error :: (Printer a) => a -> ServerError
to500Error a = err500 {errBody = buildMsg a}
  where
    buildMsg = encodeUtf8 . fromStrict . prettyPrint

-- 'serve' comes from servant and hands you a WAI Application,
-- which you can think of as an "abstract" web application,
-- not yet a webserver.
mimsaApp :: Application
mimsaApp = serve mimsaAPI mimsaServer

server :: IO ()
server = do
  T.putStrLn "Starting server on port 8081..."
  run 8081 mimsaApp
