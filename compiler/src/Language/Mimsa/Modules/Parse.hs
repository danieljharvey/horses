{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}

module Language.Mimsa.Modules.Parse (parseModule) where

import Control.Monad.Except
import Data.Bifunctor
import Data.Map.Strict (Map)
import Data.Text (Text)
import Language.Mimsa.Modules.FromParts
import qualified Language.Mimsa.Core as Parser
import Language.Mimsa.Core (Annotation, ModuleHash, Module)
import Language.Mimsa.Types.Error

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
