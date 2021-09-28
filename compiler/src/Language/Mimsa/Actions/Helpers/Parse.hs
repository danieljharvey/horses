module Language.Mimsa.Actions.Helpers.Parse (parseExpr) where

import Control.Monad.Except
import Data.Text (Text)
import Language.Mimsa.Actions.Types
import qualified Language.Mimsa.Parser as Parser
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers

parseExpr :: Text -> ActionM (Expr Name Annotation)
parseExpr
  input = case Parser.parseExprAndFormatError input of
    Right a -> pure a
    Left e -> throwError (ParseError e)
