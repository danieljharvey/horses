module Test.Data.Project
  ( testStdlib,
  )
where

import Data.Functor
import qualified Data.Text as T
import qualified Language.Mimsa.Actions.Monad as Actions
import Language.Mimsa.Core
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Project

-- check removing annotation doesn't break stuff
testStdlib :: Project Annotation
testStdlib = case buildTestStdlib of
  Right stdLib' -> (stdLib' $> ()) $> mempty
  Left e ->
    error (T.unpack $ prettyPrint e)

buildTestStdlib :: Either (Error Annotation) (Project Annotation)
buildTestStdlib =
  Actions.run mempty action >>= \(proj, _, _) -> pure proj
  where
    action = pure ()
