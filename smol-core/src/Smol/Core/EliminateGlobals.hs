{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module Smol.Core.EliminateGlobals (eliminateGlobals) where

import Control.Monad.Reader
import Control.Monad.Writer
import Data.Set (Set)
import qualified Data.Set as S
import Data.String (fromString)
import Smol.Core.ExprUtils (bindExpr)
import Smol.Core.Typecheck.Shared
import Smol.Core.Types.Expr
import Smol.Core.Types.Identifier

bigArgName :: Identifier
bigArgName = fromString "innerargs"

eliminateGlobals :: (Identifier -> dep Identifier) -> Expr dep ann -> Expr dep ann
eliminateGlobals liftIdent expr =
  let (newExpr, ElimEnv idents) =
        runWriter (runReaderT (collectAndReplaceGlobals liftIdent expr) mempty)
   in if S.null idents then newExpr else withLambda newExpr
  where
    withLambda = ELambda (getExprAnnotation expr) (liftIdent bigArgName)

newtype ElimEnv = ElimEnv {runElimEnv :: Set Identifier}
  deriving newtype (Semigroup, Monoid)

collectAndReplaceGlobals ::
  ( MonadWriter ElimEnv m,
    MonadReader ElimEnv m
  ) =>
  (Identifier -> dep Identifier) ->
  Expr dep ann ->
  m (Expr dep ann)
collectAndReplaceGlobals liftIdent = \case
  (EGlobalLet ann var expr body) -> do
    tidyExpr <- collectAndReplaceGlobals liftIdent expr
    (result, ElimEnv idents) <-
      runWriterT
        ( local
            (ElimEnv (S.singleton var) <>)
            ( ELet ann (liftIdent var) tidyExpr
                <$> collectAndReplaceGlobals liftIdent body
            )
        )
    tell (ElimEnv $ S.delete var idents)
    pure result
  (EGlobal ann var) -> do
    tell (ElimEnv (S.singleton var))
    found <- asks (S.member var . runElimEnv)
    pure $
      if found
        then EVar ann (liftIdent var) -- it's been turned into a regular let, return a var
        else ERecordAccess ann (EVar ann (liftIdent bigArgName)) var
  other -> bindExpr (collectAndReplaceGlobals liftIdent) other
