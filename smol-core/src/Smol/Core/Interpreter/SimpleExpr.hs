module Smol.Core.Interpreter.SimpleExpr (simpleExpr) where

import Data.Bifunctor
import Data.Functor
import Smol.Core.Types.Expr
import Smol.Core.Types.Pattern
import Smol.Core.Types.Spread

-- | simpleExpr
simpleExpr :: Expr dep ann -> Expr dep ()
simpleExpr (EConstructor _ tyCon) =
  EConstructor mempty tyCon
simpleExpr (EVar _ var) =
  EVar mempty var
simpleExpr (EPrim _ lit) = EPrim mempty lit
simpleExpr (EAnn _ mt expr) =
  EAnn mempty (mt $> mempty) (simpleExpr expr)
simpleExpr (ELet _ ident expr body) =
  ELet mempty ident (simpleExpr expr) (simpleExpr body)
simpleExpr (EInfix _ op a b) = EInfix mempty op (simpleExpr a) (simpleExpr b)
simpleExpr (ELambda _ ident body) = ELambda mempty ident  (simpleExpr body)
simpleExpr (EApp _ fn val) = EApp mempty (simpleExpr fn) (simpleExpr val)
simpleExpr (EIf _ predExpr thenExpr elseExpr) =
  EIf mempty (simpleExpr predExpr) (simpleExpr thenExpr) (simpleExpr elseExpr)
simpleExpr (ETuple _ a as) = ETuple mempty (simpleExpr a) (simpleExpr <$> as)
simpleExpr (ERecord _ as) = ERecord mempty (simpleExpr <$> as)
simpleExpr (ERecordAccess _ expr name) = ERecordAccess mempty (simpleExpr expr) name
simpleExpr (EArray _ as) = EArray mempty (simpleExpr <$> as)
simpleExpr (EPatternMatch _ expr pats) =
  EPatternMatch mempty (simpleExpr expr) (bimap simplePattern simpleExpr <$> pats)

simplePattern :: Pattern dep ann -> Pattern dep ()
simplePattern (PVar _ var) = PVar mempty var
simplePattern (PConstructor _ tyCon args) =
  PConstructor mempty tyCon (simplePattern <$> args)
simplePattern (PWildcard _) = PWildcard mempty
simplePattern (PTuple _ a as) = PTuple mempty (simplePattern a) (simplePattern <$> as)
simplePattern (PLiteral _ lit) = PLiteral mempty lit
simplePattern (PArray _ vals spread) =
  let simpleSpread = case spread of
        SpreadValue _ a -> SpreadValue mempty a
        SpreadWildcard _ -> SpreadWildcard mempty
        NoSpread -> NoSpread
   in PArray mempty (simplePattern <$> vals) simpleSpread
