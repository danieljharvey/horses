module Calc.TypeUtils (mapOuterTypeAnnotation, getOuterTypeAnnotation) where

import Calc.Types.Type

getOuterTypeAnnotation :: Type ann -> ann
getOuterTypeAnnotation (TPrim ann _) = ann

mapOuterTypeAnnotation :: (ann -> ann) -> Type ann -> Type ann
mapOuterTypeAnnotation f (TPrim ann p) = TPrim (f ann) p
