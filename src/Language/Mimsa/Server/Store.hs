{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Language.Mimsa.Server.Store
  ( storeEndpoints,
    StoreAPI,
  )
where

import Control.Monad.Except
import Data.Text.Lazy (fromStrict)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Language.Mimsa.Printer
import Language.Mimsa.Server.Types
import Language.Mimsa.Store.Storage (findExpr, saveExpr)
import Language.Mimsa.Types.Store
import Servant

-- the store API is used to communicate between instances of Mimsa
-- the data it outputs should be enough for another Repl or Server to use

type StoreAPI =
  "store"
    :> ( "expression"
           :> Capture "exprHash" ExprHash
           :> Get '[JSON] (StoreExpression ())
       )
    :<|> ( "expression"
             :> ReqBody '[JSON] (StoreExpression ())
             :> Post '[JSON] ExprHash
         )

storeEndpoints :: MimsaEnvironment -> Server StoreAPI
storeEndpoints mimsaEnv =
  getExpression mimsaEnv
    :<|> postExpression mimsaEnv

getExpression ::
  MimsaEnvironment ->
  ExprHash ->
  Handler (StoreExpression ())
getExpression mimsaEnv exprHash' =
  Handler $ withExceptT to500Error (findExpr (mimsaConfig mimsaEnv) exprHash')

postExpression ::
  MimsaEnvironment ->
  StoreExpression () ->
  Handler ExprHash
postExpression mimsaEnv se =
  Handler $ withExceptT to500Error (saveExpr (mimsaConfig mimsaEnv) se)

to500Error :: (Printer a) => a -> ServerError
to500Error a = err500 {errBody = buildMsg a}
  where
    buildMsg = encodeUtf8 . fromStrict . prettyPrint
