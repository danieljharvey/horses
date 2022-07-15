module Language.Mimsa.Actions.Helpers.Parse (parseExpr, parseDataType, parseModule) where

import Control.Monad.Except
import Data.Text (Text)
import Language.Mimsa.Actions.Types
import qualified Language.Mimsa.Modules.Parse as Module
import qualified Language.Mimsa.Parser as Parser
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Modules

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
