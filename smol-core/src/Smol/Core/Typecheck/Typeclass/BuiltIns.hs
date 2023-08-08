{-# LANGUAGE OverloadedStrings #-}

module Smol.Core.Typecheck.Typeclass.BuiltIns (builtInClasses, builtInInstances) where


import Data.Functor
import Smol.Core.ExprUtils
import Smol.Core.Parser
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import Smol.Core.Typecheck.Typeclass.Types
import Smol.Core.Types
import qualified Data.Text as T
import Control.Monad.Identity

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

unsafeParseExpr :: T.Text -> Expr ParseDep ()
unsafeParseExpr input = case parseExprAndFormatError input of
  Right expr -> expr $> ()
  Left e -> error (show e)


identityFromParsedExpr :: Expr ParseDep ann -> Expr Identity ann
identityFromParsedExpr = mapExprDep resolve
  where
    resolve (ParseDep a _) = Identity a

unsafeParseInstanceExpr :: (Monoid ann) => T.Text -> Expr Identity ann
unsafeParseInstanceExpr =
  fmap (const mempty) . identityFromParsedExpr . unsafeParseExpr


tyInt :: (Monoid ann) => Type dep ann
tyInt = TPrim mempty TPInt

tcVar :: (Monoid ann) => Identifier -> Type Identity ann
tcVar = TVar mempty . Identity


tyTuple ::
  (Monoid ann) =>
  Type dep ann ->
  [Type dep ann] ->
  Type dep ann
tyTuple a as = TTuple mempty a (NE.fromList as)

-- we should get rid of this once we can parse these in modules
builtInInstances :: (Ord ann, Monoid ann) => M.Map (Constraint ann) (Instance ann)
builtInInstances =
  M.fromList
    [ ( Constraint "Eq" [tyInt],
        Instance {inExpr = unsafeParseInstanceExpr "\\a -> \\b -> a == b", inConstraints = []}
      ),
      ( Constraint "Eq" [tyTuple (tcVar "a") [tcVar "b"]],
        Instance
          { inExpr =
              unsafeParseInstanceExpr "\\a -> \\b -> case (a,b) of ((a1, a2), (b1, b2)) -> if equals a1 b1 then equals a2 b2 else False",
            inConstraints =
              [ Constraint "Eq" [tcVar "a"],
                Constraint "Eq" [tcVar "b"]
              ]
          }
      )
    ]



