{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Project.Versions
  ( findVersionsSimple,
    findInProject,
  )
where

import Control.Monad.Except
import Data.Bifunctor (first)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.Store

-- versions of a binding along with numbers
findVersionsSimple ::
  Project ann ->
  Name ->
  Either
    (Error ann)
    ( NonEmpty
        ( Int,
          ExprHash
        )
    )
findVersionsSimple project name = do
  versioned <- first StoreErr (findInProject project name)
  pure . NE.reverse . NE.zip (NE.fromList [1 ..]) $ versioned

findInProject ::
  Project ann ->
  Name ->
  Either StoreError (NonEmpty ExprHash)
findInProject project name =
  case M.lookup name (getVersionedMap $ prjBindings project) of
    Just versioned -> Right versioned
    _ -> throwError $ CouldNotFindBinding name
