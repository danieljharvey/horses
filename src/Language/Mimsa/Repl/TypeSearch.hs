{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Repl.TypeSearch
  ( doTypeSearch,
  )
where

import Data.Bifunctor (first)
import Data.Foldable
import qualified Data.Map as M
import qualified Data.Text as T
import Language.Mimsa.Logging
import Language.Mimsa.Printer
import Language.Mimsa.Project
  ( getCurrentBindings,
  )
import Language.Mimsa.Project.TypeSearch
import Language.Mimsa.Repl.Types
import Language.Mimsa.Store (resolveDeps)
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.Typechecker

-------------

doTypeSearch ::
  Project Annotation -> MonoType -> ReplM Annotation ()
doTypeSearch env mt = do
  let toError names = OtherError (T.pack $ show names)
  deps <-
    liftRepl
      ( first
          toError
          ( resolveDeps
              (store env)
              (getCurrentBindings $ bindings env)
          )
      )
  matches <- liftRepl (typeSearch (debugPretty "deps" deps) (debugPretty "mt" mt))
  traverse_
    ( \(name, mt') ->
        replPrint (prettyPrint name <> ":" <> prettyPrint mt')
    )
    (M.toList matches)
