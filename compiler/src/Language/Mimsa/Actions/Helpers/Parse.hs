module Language.Mimsa.Actions.Helpers.Parse (parseExpr, parseDataType, parseModule) where

import Control.Monad.Except
import Data.Text (Text)
import Language.Mimsa.Actions.Types
import Language.Mimsa.Core (Annotation, DataType, Expr, Module, Name)
import qualified Language.Mimsa.Core as Parser
import qualified Language.Mimsa.Modules.Parse as Module
import Language.Mimsa.Types.Error

parseExpr :: Text -> ActionM (Expr Name Annotation)
parseExpr
  input = case Parser.parseExpr input of
    Right a -> pure a
    Left e -> throwError (ParseError input e)

parseDataType :: Text -> ActionM DataType
parseDataType input =
  case Parser.parseTypeDecl input of
    Right a -> pure a
    Left e -> throwError (ParseError input e)

parseModule :: Text -> ActionM (Module Annotation)
parseModule =
  Module.parseModule mempty
