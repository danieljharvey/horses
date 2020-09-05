module Language.Mimsa.Types.MonoType
  ( MonoType (..),
  )
where

import Data.Map (Map)
import Language.Mimsa.Types.Construct
import Language.Mimsa.Types.Name
import Language.Mimsa.Types.Variable

data MonoType
  = MTInt
  | MTString
  | MTBool
  | MTUnit
  | MTFunction MonoType MonoType -- argument, result
  | MTPair MonoType MonoType -- (a,b)
  | MTRecord (Map Name MonoType) -- { foo: a, bar: b }
  | MTVar Variable
  | MTData Construct [MonoType] -- name, typeVars
  deriving (Eq, Ord, Show)
