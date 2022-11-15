{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Language.Mimsa.Types.AST.HOASExpr
  ( HOASExpr (..),
  )
where

import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import Language.Mimsa.Types.AST.Identifier
import Language.Mimsa.Types.AST.Literal (Literal)
import Language.Mimsa.Types.AST.Operator
import Language.Mimsa.Types.AST.Pattern
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Modules.ModuleName
import Language.Mimsa.Types.Typechecker.MonoType

-------

instance Show (HOASExpr var ann -> HOASExpr var ann) where
  show _ = "some function"

instance Show (HOASExpr var ann -> HOASExpr var ann -> HOASExpr var ann) where
  show _ = "some recursive function"


-- |
-- version of Expr where functions are represented as functions
-- for interpreting but ZOOM
data HOASExpr var ann
  = -- | a literal, such as String, Int, Boolean
    MyLiteral
      { expAnn :: ann,
        expLit :: Literal
      }
  | MyAnnotation
      { expAnn :: ann,
        expType :: Type ann,
        expExpr :: HOASExpr var ann
      }
  | -- | a named variable
    MyVar
      { expAnn :: ann,
        expModuleName :: Maybe ModuleName,
        expVar :: var
      }
  | -- | pat, expr, body
    MyLetPattern
      { expAnn :: ann,
        expPattern :: Pattern var ann,
        expExpr :: HOASExpr var ann,
        expBodyFunc :: HOASExpr var ann -> HOASExpr var ann
      }
  | -- | a `f` b
    MyInfix
      { expAnn :: ann,
        expOperator :: Operator,
        expLeft :: HOASExpr var ann,
        expRight :: HOASExpr var ann
      }
  | -- | binder, body
    MyLambda
      { expAnn :: ann,
        expBinder :: Identifier var ann,
        expBodyFunc :: HOASExpr var ann -> HOASExpr var ann
      }
  | -- | binder, body
    MyRecursiveLambda
      { expAnn :: ann,
        expBinder :: Identifier var ann,
        expBodyRecursiveFunc :: HOASExpr var ann ->
              HOASExpr var ann -> HOASExpr var ann
      }

  | -- | function, argument
    MyApp
      { expAnn :: ann,
        expFunc :: HOASExpr var ann,
        expArg :: HOASExpr var ann
      }
  | -- | expr, thencase, elsecase
    MyIf
      { expAnn :: ann,
        expPred :: HOASExpr var ann,
        expThen :: HOASExpr var ann,
        expElse :: HOASExpr var ann
      }
  | -- | (a,b)
    MyTuple
      { expAnn :: ann,
        expA :: HOASExpr var ann,
        expAs :: NE.NonEmpty (HOASExpr var ann)
      }
  | -- | { dog: MyLiteral (MyInt 1), cat: MyLiteral (MyInt 2) }
    MyRecord
      { expAnn :: ann,
        expRecordItems :: Map Name (HOASExpr var ann)
      }
  | -- | a.foo
    MyRecordAccess
      { expAnn :: ann,
        expRecord :: HOASExpr var ann,
        expKey :: Name
      }
  | MyArray
      { expAnn :: ann,
        expArrayItems :: [HOASExpr var ann]
      }
  | -- | use a constructor by name
    MyConstructor
      { expAnn :: ann,
        expModuleName :: Maybe ModuleName,
        expTyCon :: TyCon
      }
  | -- | expr, [(pattern, expr)]
    MyPatternMatch
      { expAnn :: ann,
        expExpr :: HOASExpr var ann,
        expPatterns ::
          [ ( Pattern var ann,
              HOASExpr var ann -> HOASExpr var ann
            )
          ]
      }
  | -- | name
    MyTypedHole {expAnn :: ann, expTypedHoleName :: var}
  deriving (Show)
