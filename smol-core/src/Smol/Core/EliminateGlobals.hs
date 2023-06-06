{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
  {-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
module Smol.Core.EliminateGlobals (eliminateGlobals) where

import Smol.Core.Types.Identifier
import Smol.Core.Types.Expr
import Control.Monad.Reader
import Data.Set (Set)
import qualified Data.Set as S

eliminateGlobals :: (Identifier -> dep Identifier) -> Expr dep ann -> Expr dep ann
eliminateGlobals liftIdent expr = runReader (collectAndReplaceGlobals liftIdent expr) mempty

newtype ElimEnv = ElimEnv { runElimEnv :: Set Identifier }
  deriving newtype (Semigroup, Monoid)

collectAndReplaceGlobals :: (MonadReader ElimEnv m) => (Identifier -> dep Identifier) -> Expr dep ann -> m (Expr dep ann)
collectAndReplaceGlobals liftIdent = \case
  (EGlobalLet ann var expr body) -> do
      tidyExpr <- collectAndReplaceGlobals liftIdent expr
      local ((ElimEnv (S.singleton var)) <>)
            $ ELet ann (liftIdent var) tidyExpr <$> collectAndReplaceGlobals liftIdent body
  (EGlobal ann var) -> do
    found <- asks (S.member var . runElimEnv)
    pure $ if found then
             EVar ann (liftIdent var) -- it's been turned into a regular let, return a var
             else error "not found" -- it's referring to the big args thing
  other -> pure other
