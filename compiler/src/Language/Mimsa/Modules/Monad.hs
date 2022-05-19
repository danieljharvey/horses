{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Language.Mimsa.Modules.Monad (CheckM (..), CheckEnv (..), runCheck) where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Map (Map)
import qualified Data.Map as M
import Language.Mimsa.Modules.Prelude
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Modules.Module
import Language.Mimsa.Types.Modules.ModuleHash

-- this is where we keep all the modules we need to do things
newtype CheckEnv ann = CheckEnv
  { ceModules :: Map ModuleHash (Module ann)
  }

newtype CheckM a = CheckM
  { runCheckM ::
      ExceptT
        (Error Annotation)
        ( Reader (CheckEnv Annotation)
        )
        a
  }
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadError (Error Annotation),
      MonadReader (CheckEnv Annotation)
    )

runCheck :: CheckM a -> Either (Error Annotation) a
runCheck comp = runReader (runExceptT (runCheckM comp)) initialEnv
  where
    initialEnv =
      CheckEnv
        { ceModules = M.singleton preludeHash prelude
        }
