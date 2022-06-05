{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}

module Language.Mimsa.Modules.Parse (parseModule, parseModule') where

import Control.Monad.Except
import Data.Bifunctor
import Data.Text (Text)
import Language.Mimsa.Modules.FromParts
import Language.Mimsa.Modules.Monad
import qualified Language.Mimsa.Parser.Module as Parser
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Modules

parseModule :: Text -> Either (Error Annotation) (Module Annotation)
parseModule input = runCheck input mempty (parseModule' input)

parseModule' :: Text -> CheckM (Module Annotation)
parseModule' input = do
  moduleItems <-
    liftEither $
      first (ParseError input) (Parser.parseModule input)
  -- create module from parsed items
  moduleFromModuleParts moduleItems
