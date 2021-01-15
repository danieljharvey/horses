module Language.Mimsa.Types.Project.UnitTest where

import Data.Text (Text)
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers

newtype TestName = TestName Text

data UnitTest ann = UnitTest TestName (Expr Name ann)
