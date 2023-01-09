module Language.Mimsa.Actions.Modules.ToStoreExpressions
  ( toStoreExpressions,
    lookupByName,
  )
where

import Control.Monad.Except
import Data.Foldable
import qualified Data.Map.Strict as M
import qualified Language.Mimsa.Actions.Modules.Typecheck as Actions
import qualified Language.Mimsa.Actions.Monad as Actions
import Language.Mimsa.Core
import Language.Mimsa.Modules.HashModule
import Language.Mimsa.Modules.ToStoreExprs (CompiledModule (..))
import qualified Language.Mimsa.Modules.ToStoreExprs as Modules
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Store

toStoreExpressions ::
  Module MonoType ->
  Actions.ActionM (CompiledModule MonoType)
toStoreExpressions localModule = do
  typecheckedModules <- Actions.typecheckModules (prettyPrint localModule) (getAnnotationForType <$> localModule)

  let (_, rootModuleHash) = serializeModule localModule

  -- pull root module out from pile of typechecked modules
  typecheckedModule <- case M.lookup rootModuleHash typecheckedModules of
    Just tcMod -> pure tcMod
    _ -> throwError (ModuleErr $ MissingModule rootModuleHash)

  -- compile to store expressions
  compiledModule <- Modules.toStoreExpressions typecheckedModules typecheckedModule

  -- need to get our new store items into the project so this works I reckon
  traverse_
    (Actions.appendStoreExpression . fmap getAnnotationForType)
    (getStore $ cmStore compiledModule)

  pure compiledModule

-- TODO: real errors
lookupByName ::
  CompiledModule ann ->
  DefIdentifier ->
  Actions.ActionM (StoreExpression ann)
lookupByName compiled defId =
  -- find the root StoreExpression by name
  case M.lookup defId (cmExprs compiled)
    >>= flip M.lookup (getStore $ cmStore compiled) of
    Just se -> pure se
    _ -> error $ "lookupByName: could not find " <> show defId <> " in compiled store expressions"
