{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}

module Smol.Core.Interpreter.Types
  ( IExpr (..),
  )
where

import Control.Monad.Identity
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict
import Data.Sequence (Seq)
import Smol.Core.Types

-- | An IExpr is an Expr with a function Lambda
data IExpr ann
  = ILambda
      ann
      Identifier
      (IExpr ann -> IExpr ann)
  | IInfix ann Op (IExpr ann) (IExpr ann)
  | IPrim ann Prim
  | IApp ann (IExpr ann) (IExpr ann)
  | IIf ann (IExpr ann) (IExpr ann) (IExpr ann)
  | IAnn ann (Type Identity ann) (IExpr ann)
  | IVar ann Identifier
  | IConstructor ann Constructor
  | ITuple
      ann
      (IExpr ann)
      (NE.NonEmpty (IExpr ann))
  | IArray ann (Seq (IExpr ann))
  | IRecord ann (Map Identifier (IExpr ann))
  | IRecordAccess ann (IExpr ann) Identifier
  | IPatternMatch
      ann
      (IExpr ann)
      (NE.NonEmpty (Pattern Identity ann, IExpr ann -> IExpr ann))
