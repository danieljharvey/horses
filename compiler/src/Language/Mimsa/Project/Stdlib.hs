module Language.Mimsa.Project.Stdlib (buildStdlib, stdlib) where

import qualified Language.Mimsa.Actions.Monad as Actions
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Project

buildStdlib :: Either (Error Annotation) (Project Annotation)
buildStdlib =
  Actions.run mempty action >>= \(proj, _, _) -> pure proj
  where
    action = pure ()

stdlib :: Project Annotation
stdlib = case buildStdlib of
  Right a -> a
  Left e -> error (show e)
