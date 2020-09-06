module Language.Mimsa.Printer.Construct where

import Data.Text.Prettyprint.Doc
import Language.Mimsa.Types.Construct

renderConstruct :: Construct -> Doc ann
renderConstruct = pretty . getConstruct
