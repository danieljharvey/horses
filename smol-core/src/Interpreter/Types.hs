{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}

module Interpreter.Types
  ( IExpr (..),
  )
where

import qualified Data.List.NonEmpty as NE
import Data.Map.Strict
import Types

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
  | IAnn ann (Type ann) (IExpr ann)
  | IVar ann Identifier
  | IConstructor ann Constructor
  | ITuple
      ann
      (IExpr ann)
      (NE.NonEmpty (IExpr ann))
  | IGlobal ann Identifier
  | IGlobalLet ann Identifier (IExpr ann) (IExpr ann)
  | IRecord ann (Map Identifier (IExpr ann))
  | IRecordAccess ann (IExpr ann) Identifier
  | IPatternMatch
      ann
      (IExpr ann)
      (NE.NonEmpty (Pattern ann, IExpr ann -> IExpr ann))
