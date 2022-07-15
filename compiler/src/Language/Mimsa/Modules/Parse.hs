{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}

module Language.Mimsa.Modules.Parse (parseModule) where

import Control.Monad.Except
import Data.Bifunctor
import Data.Map (Map)
import Data.Text (Text)
import Language.Mimsa.Modules.FromParts
import qualified Language.Mimsa.Parser.Module as Parser
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Modules

parseModule ::
  (MonadError (Error Annotation) m) =>
  Map ModuleHash (Module Annotation) ->
  Text ->
  m (Module Annotation)
parseModule modules input = do
  moduleItems <-
    liftEither $
      first (ParseError input) (Parser.parseModule input)
  -- create module from parsed items
  moduleFromModuleParts modules moduleItems
