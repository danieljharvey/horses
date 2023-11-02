module Smol.Core.Modules.PrettyPrint (printModuleParts) where

import Data.Foldable (foldl')
import qualified Prettyprinter as PP
import Smol.Core.Modules.Types.ModuleItem
import Smol.Core.Printer

printModuleParts :: [ModuleItem ann] -> PP.Doc a
printModuleParts = foldl' (\doc a -> doc PP.<+> prettyDoc a) mempty