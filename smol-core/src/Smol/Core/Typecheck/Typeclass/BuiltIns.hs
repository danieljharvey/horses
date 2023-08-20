{-# LANGUAGE OverloadedStrings #-}

module Smol.Core.Typecheck.Typeclass.BuiltIns (builtInClasses, builtInInstances) where

import Data.Functor
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Smol.Core.Parser
import Smol.Core.Typecheck.FromParsedExpr
import Smol.Core.Typecheck.Typeclass.Types
import Smol.Core.Types

showTypeclass :: (Monoid ann) => Typeclass ResolvedDep ann
showTypeclass =
  Typeclass
    { tcName = "Show",
      tcArgs = ["a"],
      tcFuncName = "show",
      tcFuncType = TFunc mempty mempty (TVar mempty "a") (TPrim mempty TPString)
    }

eqTypeclass :: (Monoid ann) => Typeclass ResolvedDep ann
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

builtInClasses :: (Monoid ann) => M.Map TypeclassName (Typeclass ResolvedDep ann)
builtInClasses =
  M.fromList
    [ ("Eq", eqTypeclass),
      ("Show", showTypeclass)
    ]

-----

unsafeParseExpr :: T.Text -> Expr ParseDep ()
unsafeParseExpr input = case parseExprAndFormatError input of
  Right expr -> expr $> ()
  Left e -> error (show e)

unsafeParseMemptyExpr :: (Monoid ann) => T.Text -> Expr ResolvedDep ann
unsafeParseMemptyExpr =
  fmap (const mempty) . fromParsedExpr . unsafeParseExpr

tyInt :: (Monoid ann) => Type dep ann
tyInt = TPrim mempty TPInt

tyVar :: (Monoid ann) => Identifier -> Type ResolvedDep ann
tyVar = TVar mempty . LocalDefinition

tyTuple ::
  (Monoid ann) =>
  Type dep ann ->
  [Type dep ann] ->
  Type dep ann
tyTuple a as = TTuple mempty a (NE.fromList as)

-- we should get rid of this once we can parse these in modules
builtInInstances :: (Ord ann, Monoid ann) => M.Map (Constraint ResolvedDep ann) (Instance ResolvedDep ann)
builtInInstances =
  M.fromList
    [ ( Constraint "Eq" [tyInt],
        Instance {inExpr = unsafeParseMemptyExpr "\\a -> \\b -> a == b", inConstraints = []}
      ),
      ( Constraint "Eq" [tyTuple (tyVar "a") [tyVar "b"]],
        Instance
          { inExpr =
              unsafeParseMemptyExpr "\\a -> \\b -> case (a,b) of ((a1, a2), (b1, b2)) -> if equals a1 b1 then equals a2 b2 else False",
            inConstraints =
              [ Constraint "Eq" [tyVar "a"],
                Constraint "Eq" [tyVar "b"]
              ]
          }
      )
    ]
