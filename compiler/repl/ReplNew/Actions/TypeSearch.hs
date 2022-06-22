{-# LANGUAGE OverloadedStrings #-}

module ReplNew.Actions.TypeSearch
  ( doTypeSearch,
  )
where

import Data.Foldable
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Language.Mimsa.Actions.Monad as Actions
import qualified Language.Mimsa.Actions.Typecheck as Actions
import Language.Mimsa.Printer
import Language.Mimsa.Project.TypeSearch
import Language.Mimsa.Typechecker.NormaliseTypes (normaliseType)
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.Typechecker
import ReplNew.ReplM

-------------

doTypeSearch ::
  Project Annotation -> MonoType -> ReplM (Error Annotation) ()
doTypeSearch project mt = do
  (_, _, typeMap) <- replMFromEither $ Actions.run project Actions.typeMapForProjectSearch
  let matches = typeSearch typeMap mt
  let simplified = normaliseType mt
  case M.toList matches of
    [] ->
      replOutput $ "Could not find a type match for " <> prettyPrint simplified
    as -> do
      replOutput $ (T.pack . show . length) as <> " matches for " <> prettyPrint simplified
      traverse_
        ( \(name, mt') ->
            replOutput (prettyPrint name <> ": " <> prettyPrint mt')
        )
        as
