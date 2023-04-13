module Calc.PatternUtils (getPatternAnnotation) where

import Calc.Types.Pattern

getPatternAnnotation :: Pattern ann -> ann
getPatternAnnotation (PLiteral ann _) = ann
getPatternAnnotation (PWildcard ann) = ann
getPatternAnnotation (PVar ann _ ) = ann
getPatternAnnotation (PTuple ann _ _) = ann
