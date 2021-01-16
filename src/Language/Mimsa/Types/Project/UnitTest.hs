{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Language.Mimsa.Types.Project.UnitTest where

import Data.Set (Set)
import Data.Text (Text)
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Store

newtype TestName = TestName Text
  deriving newtype (Eq, Ord, Show)

newtype TestSuccess = TestSuccess Bool
  deriving newtype (Eq, Ord, Show)

data UnitTest ann = UnitTest
  { utName :: TestName,
    utSuccess :: TestSuccess,
    utExpr :: Expr Name ann,
    utDeps :: Set ExprHash
  }
  deriving stock (Eq, Ord, Show)
