module Calc.TypeUtils (mapOuterTypeAnnotation, getOuterTypeAnnotation) where

import Calc.Types.Type

getOuterTypeAnnotation :: Type ann -> ann
getOuterTypeAnnotation (TPrim ann _) = ann
getOuterTypeAnnotation (TFunction ann _ _) = ann

mapOuterTypeAnnotation :: (ann -> ann) -> Type ann -> Type ann
mapOuterTypeAnnotation f (TPrim ann p) = TPrim (f ann) p
mapOuterTypeAnnotation f (TFunction ann a b) = TFunction (f ann) a b
