module Calc.TypeUtils (mapOuterTypeAnnotation) where

import Calc.Types.Type

mapOuterTypeAnnotation :: (ann -> ann) -> Type ann -> Type ann
mapOuterTypeAnnotation f (TPrim ann p) = TPrim (f ann) p
