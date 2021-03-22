module Language.Mimsa.Types.Store.ResolvedTypeDeps where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Text.Prettyprint.Doc
import Language.Mimsa.Printer
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Store.ExprHash

newtype ResolvedTypeDeps ann = ResolvedTypeDeps
  { getResolvedTypeDeps ::
      Map
        TyCon
        (ExprHash, DataType ann)
  }

instance Printer (ResolvedTypeDeps ann) where
  prettyDoc (ResolvedTypeDeps deps) =
    encloseSep
      lbrace
      rbrace
      comma
      (prettyDoc <$> M.keys deps)
