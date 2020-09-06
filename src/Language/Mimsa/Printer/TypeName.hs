module Language.Mimsa.Printer.TypeName where

import Data.Text.Prettyprint.Doc
import Language.Mimsa.Printer.Construct
import Language.Mimsa.Printer.Name
import Language.Mimsa.Types.TypeName

renderTypeName :: TypeName -> Doc ann
renderTypeName (ConsName c []) = renderConstruct c
renderTypeName (ConsName c tys) =
  encloseSep
    lparen
    rparen
    space
    ([renderConstruct c] <> (renderTypeName <$> tys))
renderTypeName (VarName v) = renderName v
