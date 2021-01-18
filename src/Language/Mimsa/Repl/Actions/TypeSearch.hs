{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Repl.Actions.TypeSearch
  ( doTypeSearch,
  )
where

import Data.Foldable
import qualified Data.Map as M
import qualified Data.Text as T
import Language.Mimsa.Actions
import Language.Mimsa.Printer
import Language.Mimsa.Project.TypeSearch
import Language.Mimsa.Repl.Types
import Language.Mimsa.Typechecker.NormaliseTypes (normaliseType)
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.Typechecker

-------------

doTypeSearch ::
  Project Annotation -> MonoType -> ReplM Annotation ()
doTypeSearch env mt = do
  typeMap <- liftRepl $ getTypeMap env
  let matches = typeSearch typeMap mt
  let simplified = normaliseType mt
  case M.toList matches of
    [] ->
      replPrint $ "Could not find a type match for " <> prettyPrint simplified
    as -> do
      replPrint $ (T.pack . show . length) as <> " matches for " <> prettyPrint simplified
      traverse_
        ( \(name, mt') ->
            replPrint (prettyPrint name <> ": " <> prettyPrint mt')
        )
        as
