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
import Language.Mimsa.Monad
import Language.Mimsa.Printer
import Language.Mimsa.Server.Helpers
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
getExpression mimsaEnv exprHash' = do
  let mimsaCfg = mimsaConfig mimsaEnv
  handleMimsaM mimsaCfg UserError (findExpr exprHash')

postExpression ::
  MimsaEnvironment ->
  StoreExpression () ->
  Handler ExprHash
postExpression mimsaEnv se = do
  let mimsaCfg = mimsaConfig mimsaEnv
  handleMimsaM mimsaCfg InternalError (saveExpr se)
