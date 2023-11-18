module Smol.Modules.PrettyPrint (printModuleParts) where

import Data.Foldable (foldl')
import qualified Prettyprinter as PP
import Smol.Core.Printer
import Smol.Modules.Types.ModuleItem

printModuleParts :: [ModuleItem ann] -> PP.Doc a
printModuleParts = foldl' (\doc a -> doc <> prettyDoc a) mempty
