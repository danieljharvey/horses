module Language.Mimsa.Types.Store.ResolvedTypeDeps where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Language.Mimsa.Core
import Prettyprinter

newtype ResolvedTypeDeps = ResolvedTypeDeps
  { getResolvedTypeDeps ::
      Map
        (Maybe ModuleName, TyCon)
        DataType
  }

instance Printer ResolvedTypeDeps where
  prettyDoc (ResolvedTypeDeps deps) =
    encloseSep
      lbrace
      rbrace
      comma
      (prettyDoc <$> M.keys deps)
