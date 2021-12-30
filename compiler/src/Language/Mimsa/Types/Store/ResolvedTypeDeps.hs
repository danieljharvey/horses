module Language.Mimsa.Types.Store.ResolvedTypeDeps where

import Data.Map (Map)
import qualified Data.Map as M
import Language.Mimsa.Printer
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Store.ExprHash
import Prettyprinter

newtype ResolvedTypeDeps = ResolvedTypeDeps
  { getResolvedTypeDeps ::
      Map
        TypeName
        (ExprHash, DataType)
  }

instance Printer ResolvedTypeDeps where
  prettyDoc (ResolvedTypeDeps deps) =
    encloseSep
      lbrace
      rbrace
      comma
      (prettyDoc <$> M.keys deps)
