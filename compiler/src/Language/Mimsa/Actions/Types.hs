{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Mimsa.Actions.Types
  ( ActionM (..),
    SavePath (..),
    SaveContents (..),
    SaveFilename (..),
    ActionOutcome (..),
    ActionState (..),
  )
where

import Control.Monad.Except
import Control.Monad.State
import Data.Hashable
import Data.Map.Strict (Map)
import Data.Text (Text)
import qualified Data.Text as T
import Language.Mimsa.Core
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.Store

newtype SavePath = SavePath Text
  deriving newtype (Eq, Ord, Hashable)

instance Show SavePath where
  show (SavePath s) = T.unpack s

newtype SaveContents = SaveContents Text
  deriving newtype (Eq, Ord, Show, Hashable)

newtype SaveFilename = SaveFilename Text
  deriving newtype (Eq, Ord, Hashable)

instance Show SaveFilename where
  show (SaveFilename s) = T.unpack s

data ActionOutcome
  = NewMessage Text
  | NewStoreExpression (StoreExpression Annotation)
  | NewWriteFile SavePath SaveFilename SaveContents
  | NewModule (Module Annotation)
  deriving stock (Eq, Ord, Show)

data ActionState = ActionState
  { asProject :: Project Annotation,
    asCachedTypechecked :: Map ExprHash (StoreExpression (Type Annotation)),
    asActionOutcomes :: [ActionOutcome]
  }
  deriving stock (Eq, Ord, Show)

newtype ActionM a = ActionM
  { runActionM ::
      ExceptT
        (Error Annotation)
        (State ActionState)
        a
  }
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadError (Error Annotation),
      MonadState ActionState
    )
