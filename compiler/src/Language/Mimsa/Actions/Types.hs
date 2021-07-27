{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Mimsa.Actions.Types
  ( ActionM,
    SavePath (..),
    SaveContents (..),
    SaveFilename (..),
    ActionOutcome (..),
  )
where

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Writer
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Text (Text)
import qualified Data.Text as T
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.Store

newtype SavePath = SavePath Text
  deriving newtype (Eq, Ord)

instance Show SavePath where
  show (SavePath s) = T.unpack s

newtype SaveContents = SaveContents LBS.ByteString
  deriving newtype (Eq, Ord, Show)

newtype SaveFilename = SaveFilename LBS.ByteString
  deriving newtype (Eq, Ord)

instance Show SaveFilename where
  show (SaveFilename s) = LB.unpack s

data ActionOutcome
  = NewMessage Text
  | NewStoreExpression (StoreExpression Annotation)
  | NewWriteFile SavePath SaveFilename SaveContents
  deriving stock (Eq, Ord, Show)

type ActionM =
  ExceptT
    (Error Annotation)
    (WriterT [ActionOutcome] (State (Project Annotation)))
