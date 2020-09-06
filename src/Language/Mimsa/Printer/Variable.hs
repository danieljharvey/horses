{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Printer.Variable
  ( renderVariable,
  )
where

import Data.Text.Prettyprint.Doc
import Language.Mimsa.Printer.Name
import Language.Mimsa.Types.Variable

renderVariable :: Variable -> Doc ann
renderVariable (NamedVar n) = renderName n
renderVariable (NumberedVar i) = "U" <> pretty i
renderVariable (BuiltIn n) = renderName n
renderVariable (BuiltInActual n _) = renderName n
