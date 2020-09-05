{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Language.Mimsa.Types.Expr
  ( Expr (..),
  )
where

import qualified Data.Aeson as JSON
import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import GHC.Generics
import Language.Mimsa.Types.Construct
import Language.Mimsa.Types.DataType
import Language.Mimsa.Types.Literal
import Language.Mimsa.Types.Name

-------

data Expr a
  = MyLiteral Literal
  | MyVar a
  | MyLet a (Expr a) (Expr a) -- binder, expr, body
  | MyLetPair a a (Expr a) (Expr a) -- binderA, binderB, expr, body
  | MyLambda a (Expr a) -- binder, body
  | MyApp (Expr a) (Expr a) -- function, argument
  | MyIf (Expr a) (Expr a) (Expr a) -- expr, thencase, elsecase
  | MyPair (Expr a) (Expr a) -- (a,b)
  | MyRecord (Map Name (Expr a)) -- { dog: MyLiteral (MyInt 1), cat: MyLiteral (MyInt 2) }
  | MyRecordAccess (Expr a) Name -- a.foo
  | MyData DataType (Expr a) -- tyName, tyArgs, Map constructor args, body
  | MyConstructor Construct -- use a constructor by name
  | MyConsApp (Expr a) (Expr a) -- constructor, value
  | MyCaseMatch (Expr a) (NonEmpty (Construct, Expr a)) (Maybe (Expr a)) -- expr, matches, catchAll
  deriving (Eq, Ord, Show, Generic, JSON.FromJSON, JSON.ToJSON)
