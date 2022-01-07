{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Actions.Upgrade where

import Control.Applicative
import Control.Monad.Except
import Data.Bifunctor (first)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Language.Mimsa.Actions.Helpers.CheckStoreExpression as Actions
import qualified Language.Mimsa.Actions.Monad as Actions
import qualified Language.Mimsa.Actions.Shared as Actions
import Language.Mimsa.Printer
import Language.Mimsa.Project.Helpers
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.Store

-- | things that could happen
data UpgradeResult
  = NoDependencies
  | AlreadyUpToDate
  | Updated (StoreExpression Annotation) (Map ExprHash (NameOrTyCon, ExprHash))
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

  -- TODO: maybe make an update error
  if S.null depHashes
    then pure NoDependencies
    else do
      let replacements = replaceHashes project depHashes
      if M.null replacements
        then pure AlreadyUpToDate
        else do
          let newStoreExpr = replaceDeps replacements storeExpr

          -- check it still typechecks
          _ <-
            Actions.checkStoreExpression
              (prettyPrint (storeExpression newStoreExpr))
              project
              newStoreExpr

          -- add new expression to the project
          Actions.bindStoreExpression newStoreExpr bindingName

          -- output for logging and repl
          Actions.appendMessage
            ( "Updated " <> prettyPrint bindingName
                <> ". "
                <> replacementsMessage replacements
            )

          pure $
            Updated
              (replaceDeps replacements storeExpr)
              replacements

replacementsMessage :: Map a (NameOrTyCon, a) -> Text
replacementsMessage items =
  let size = M.size
      itemNames = fmap (prettyPrint . fst) . M.elems
      depWord i = if size i > 1 then "dependencies" else "dependency"
   in prettyPrint (size items) <> " " <> depWord items <> " updated (" <> T.intercalate ", " (itemNames items) <> ")"

-- given a list of ExprHashes, return a list of replacements
replaceHashes ::
  Project ann ->
  Set ExprHash ->
  Map ExprHash (NameOrTyCon, ExprHash)
replaceHashes project =
  M.fromList
    . mapMaybe
      ( \exprHash ->
          let maybeValue =
                fmap
                  (first Left)
                  (findNewestVersionFromHash project exprHash)
              maybeType =
                fmap
                  (first Right)
                  (findNewestTypeVersionFromHash project exprHash)
           in (,) exprHash <$> (maybeValue <|> maybeType)
      )
    . S.toList

type NameOrTyCon = Either Name TyCon

-- | given a ExprHash, try and find it amongst the project
-- bindings.
findNewestVersionFromHash ::
  Project ann ->
  ExprHash ->
  Maybe (Name, ExprHash)
findNewestVersionFromHash project exprHash =
  let getValueName =
        findAnyBindingNameForExprHash exprHash
      lookupHash k map' =
        M.lookup k map'
          >>= \eh -> if eh == exprHash then Nothing else Just eh
   in case getValueName project of
        Just name ->
          let (Bindings bindings) = getCurrentBindings (prjBindings project)
           in (,) name
                <$> lookupHash name bindings
        Nothing -> Nothing

-- | given a ExprHash, try and find it amongst the project
-- type bindings.
findNewestTypeVersionFromHash ::
  Project ann ->
  ExprHash ->
  Maybe (TyCon, ExprHash)
findNewestTypeVersionFromHash project exprHash =
  let getTypeName =
        findAnyTypeBindingNameForExprHash exprHash
      lookupHash k map' =
        M.lookup k map'
          >>= \eh -> if eh == exprHash then Nothing else Just eh
   in case getTypeName project of
        Just tyCon ->
          let (TypeBindings bindings) = getCurrentTypeBindings (prjTypeBindings project)
           in (,) tyCon
                <$> lookupHash tyCon bindings
        Nothing -> Nothing

-- given a StoreExpression and a set of hashes to replace, replace them,
-- returning a new StoreExpression
replaceDeps ::
  Map ExprHash (NameOrTyCon, ExprHash) ->
  StoreExpression ann ->
  StoreExpression ann
replaceDeps replacements se =
  let replace oldHash = maybe oldHash snd (M.lookup oldHash replacements)
      newBindings = replace <$> getBindings (storeBindings se)
      newTypeBindings = replace <$> getTypeBindings (storeTypeBindings se)
   in se
        { storeBindings = Bindings newBindings,
          storeTypeBindings = TypeBindings newTypeBindings
        }
