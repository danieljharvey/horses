module Calc.Parser.Module (moduleParser) where

import Calc.Parser.Expr
import Calc.Parser.Function
import Calc.Parser.Types
import Calc.Types.Annotation
import Calc.Types.Module
import Text.Megaparsec

moduleParser :: Parser (Module Annotation)
moduleParser = do
  funcs <- many functionParser
  Module funcs <$> exprParser
