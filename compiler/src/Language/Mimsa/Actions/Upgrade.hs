{-# LANGUAGE DerivingStrategies #-}

module Language.Mimsa.Actions.Upgrade where

import Control.Applicative
import Control.Monad.Except
import qualified Data.Map as M
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
-- import Control.Monad.Except (liftEither)
import qualified Language.Mimsa.Actions.Monad as Actions
import qualified Language.Mimsa.Actions.Shared as Actions
import Language.Mimsa.Logging
import Language.Mimsa.Project.Helpers
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.Store

-- | things that could happen
data UpgradeResult
  = NoDependencies
  | AlreadyUpToDate
  deriving stock (Eq, Ord, Show)

-- takes a store expression and upgrade it's dependencies to the newest versions
-- for now, we will just try the newest possible versions of every binding
upgradeByName ::
  Name ->
  Actions.ActionM UpgradeResult
upgradeByName bindingName = do
  project <- Actions.getProject
  exprHash <- case lookupBindingName project bindingName of
    Nothing ->
      throwError (StoreErr (CouldNotFindBinding bindingName))
    Just exprHash -> pure exprHash
  storeExpr <- Actions.lookupExpressionInStore (prjStore project) exprHash
  let depHashes = getDependencyHashes storeExpr

  if S.null depHashes
    then pure NoDependencies
    else do
      let replacements = replaceHashes project depHashes
      if S.null (debugPretty "replacements" replacements)
        then pure AlreadyUpToDate
        else error "oh no"

-- given a list of ExprHashes, return a list of replacements
replaceHashes :: Project ann -> Set ExprHash -> Set (ExprHash, ExprHash)
replaceHashes project =
  S.fromList
    . mapMaybe
      ( \exprHash ->
          (,) exprHash
            <$> findNewestVersionFromHash project exprHash
      )
    . S.toList

-- | given a ExprHash, try and find it amongst the project
-- bindings. if it is there, return the newest version, if not,
-- return the existing ExprHash
findNewestVersionFromHash ::
  Project ann ->
  ExprHash ->
  Maybe ExprHash
findNewestVersionFromHash project exprHash =
  let getValueName =
        listToMaybe . S.toList . M.keysSet
          . findBindingNameForExprHash exprHash
      getTypeName =
        listToMaybe . S.toList . M.keysSet . findTypeBindingNameForExprHash exprHash
      lookupHash k map' =
        M.lookup k map'
          >>= \eh -> if eh == exprHash then Nothing else Just eh
      fromValue = case getValueName project of
        Just name ->
          let (Bindings bindings) = getCurrentBindings (prjBindings project)
           in lookupHash name bindings
        Nothing -> Nothing
      fromTypeValue = case getTypeName project of
        Just tyCon ->
          let (TypeBindings bindings) = getCurrentTypeBindings (prjTypeBindings project)
           in lookupHash tyCon bindings
        Nothing -> Nothing
   in fromValue <|> fromTypeValue

-- | TODO: complete this
-- given a StoreExpression and a set of hashes to replace, replace them,
-- returning a new StoreExpression
replaceDeps :: Set (ExprHash, ExprHash) -> StoreExpression ann -> StoreExpression ann
replaceDeps _replacements se =
  se

{-
resolvedExpr <-
  liftEither $ Actions.getTypecheckedStoreExpression input project expr
let storeExpr = reStoreExpression resolvedExpr
Actions.appendStoreExpression storeExpr
test <- liftEither $ createTest project storeExpr testName
Actions.appendProject (fromTest test storeExpr)
pure test -}
