{-# LANGUAGE OverloadedStrings #-}

module Smol.Core.Typecheck.Typeclass.BuiltIns (builtInClasses, builtInInstances) where

import qualified Data.Map.Strict as M
import Smol.Core.Typecheck.Typeclass.Types
import Smol.Core.Types

showTypeclass :: (Monoid ann) => Typeclass ann
showTypeclass =
  Typeclass
    { tcName = "Show",
      tcArgs = ["a"],
      tcFuncName = "show",
      tcFuncType = TFunc mempty mempty (TVar mempty "a") (TPrim mempty TPString)
    }

eqTypeclass :: (Monoid ann) => Typeclass ann
eqTypeclass =
  Typeclass
    { tcName = "Eq",
      tcArgs = ["a"],
      tcFuncName = "equals",
      tcFuncType =
        TFunc
          mempty
          mempty
          (TVar mempty "a")
          (TFunc mempty mempty (TVar mempty "a") (TPrim mempty TPBool))
    }

builtInClasses :: (Monoid ann) => M.Map TypeclassName (Typeclass ann)
builtInClasses =
  M.fromList
    [ ("Eq", eqTypeclass),
      ("Show", showTypeclass)
    ]

-----

-- we should get rid of this once we can parse these in modules

builtInInstances :: (Ord ann, Monoid ann) => M.Map (Constraint ann) (Instance ann)
builtInInstances =
  M.fromList
    [ ( Constraint "Eq" [TPrim mempty TPInt],
        Instance
          { inExpr = ELambda mempty "a" (ELambda mempty "b" (EInfix mempty OpEquals (EVar mempty "a") (EVar mempty "b"))),
            inConstraints = []
          }
      )
    ]
