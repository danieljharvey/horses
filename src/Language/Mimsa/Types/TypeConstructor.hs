module Language.Mimsa.Types.TypeConstructor where

import Language.Mimsa.Types.Construct
import Language.Mimsa.Types.MonoType

data TypeConstructor
  = TypeConstructor
      { tcConsName :: Construct,
        tcTypeVars :: [MonoType],
        tcConsTypes :: [MonoType]
      }
