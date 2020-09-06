module Language.Mimsa.Types.Error where

import Data.Text (Text)
import Language.Mimsa.Types.InterpreterError
import Language.Mimsa.Types.ResolverError
import Language.Mimsa.Types.TypeError
import Language.Mimsa.Types.Usage

data Error
  = TypeErr TypeError
  | ResolverErr ResolverError
  | InterpreterErr InterpreterError
  | UsageErr UsageError
  | ParseErr Text
  | OtherError Text
  deriving (Eq, Ord, Show)
