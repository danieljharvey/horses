module Language.Mimsa.Printer.Name where

import Data.Text.Prettyprint.Doc
import Language.Mimsa.Types.Name

renderName :: Name -> Doc ann
renderName = pretty . getName
