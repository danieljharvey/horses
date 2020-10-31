{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types.ResolvedTypeDeps where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Text.Prettyprint.Doc
import Language.Mimsa.Printer
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.ExprHash
import Language.Mimsa.Types.Identifiers

newtype ResolvedTypeDeps
  = ResolvedTypeDeps
      { getResolvedTypeDeps ::
          Map TyCon
            (ExprHash, DataType)
      }

instance Printer ResolvedTypeDeps where
  prettyDoc (ResolvedTypeDeps deps) =
    encloseSep
      lbrace
      rbrace
      comma
      (prettyDoc <$> M.keys deps)
