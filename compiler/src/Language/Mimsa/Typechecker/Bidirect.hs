{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Language.Mimsa.Typechecker.Bidirect
  ( infer,
    InferM,
    ExpSmall (..),
    expAnn,
  )
where

import Control.Applicative
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Coerce
import Data.Functor
import qualified Data.Map as M
import qualified Data.Set as S
import Language.Mimsa.Logging
import Language.Mimsa.Typechecker.DataTypes
import Language.Mimsa.Typechecker.TcMonad
import Language.Mimsa.Typechecker.Unify
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Swaps
import Language.Mimsa.Types.Typechecker

type InferM = ExceptT TypeError (ReaderT Swaps (State TypecheckState))

pushType :: (MonadState TypecheckState m) => MonoType -> m ()
pushType mt = modify (\s -> s {tcsTypeStack = [mt] <> tcsTypeStack s})

popType :: (MonadState TypecheckState m) => m (Maybe MonoType)
popType = do
  stack <- gets tcsTypeStack
  case stack of
    (mtHead : mtTail) -> do
      modify (\s -> s {tcsTypeStack = mtTail})
      pure (Just mtHead)
    _ -> pure Nothing

type TcExpr = ExpSmall Variable Annotation

type ElabExpr = ExpSmall Variable MonoType

data ExpSmall var ann
  = Lit ann Literal
  | Lambda ann var (ExpSmall var ann)
  | Let ann var (ExpSmall var ann) (ExpSmall var ann)
  | App ann (ExpSmall var ann) (ExpSmall var ann)
  | Var ann var
  | Ann ann (Type ann) (ExpSmall var ann)
  | If ann (ExpSmall var ann) (ExpSmall var ann) (ExpSmall var ann)
  | Constructor ann TyCon
  deriving stock (Eq, Ord, Show, Functor)

instance Substitutable (ExpSmall Variable MonoType) where
  applySubst subst elabExpr =
    applySubst subst <$> elabExpr

expAnn :: ExpSmall var ann -> ann
expAnn (Lit ann _) = ann
expAnn (Lambda ann _ _) = ann
expAnn (Let ann _ _ _) = ann
expAnn (App ann _ _) = ann
expAnn (Var ann _) = ann
expAnn (Ann ann _ _) = ann
expAnn (If ann _ _ _) = ann
expAnn (Constructor ann _) = ann

inferLiteral :: Annotation -> Literal -> InferM ElabExpr
inferLiteral ann lit =
  let tyLit = case lit of
        (MyInt _) -> MTInt
        (MyBool _) -> MTBool
        (MyString _) -> MTString
   in pure (Lit (MTPrim ann tyLit) lit)

inferLet ::
  Environment ->
  Annotation ->
  Variable ->
  TcExpr ->
  TcExpr ->
  InferM ElabExpr
inferLet env ann binder bindExpr' inExpr = do
  let desugaredExpr =
        App
          ann
          ( Lambda
              (expAnn bindExpr')
              binder
              inExpr
          )
          bindExpr'
  -- returns different expr shape, ignore for now
  infer env desugaredExpr

lookupInEnv ::
  Swaps ->
  Variable ->
  Environment ->
  Maybe Scheme
lookupInEnv swaps var' (Environment env' _ _) =
  let look v = M.lookup v env'
      wrapName (Name n) = TVName (coerce n)
   in look (variableToTypeIdentifier var')
        <|> (M.lookup var' swaps >>= look . wrapName)

envFromVar :: Variable -> Scheme -> Environment
envFromVar binder scheme =
  Environment (M.singleton (variableToTypeIdentifier binder) scheme) mempty mempty

inferVarFromScope ::
  Environment ->
  Annotation ->
  Variable ->
  InferM ElabExpr
inferVarFromScope env ann var' = do
  swaps <- ask
  case lookupInEnv swaps var' env of
    Just (Scheme _ mt) ->
      pure (Var mt var')
    _ -> do
      throwError $
        VariableNotInEnv
          swaps
          ann
          var'
          (S.fromList (M.keys (getSchemes env)))

inferApp ::
  Environment ->
  Annotation ->
  TcExpr ->
  TcExpr ->
  InferM ElabExpr
inferApp env _ann fn arg = do
  typedArg <- infer env arg
  -- remember type
  pushType (debugPretty "inferred arg" (expAnn typedArg))
  typedFn <- infer env fn
  case debugPretty "inferred fn" (expAnn typedFn) of
    MTFunction _ mtArg mtReturn -> do
      let typedArg' = typedArg $> mtArg
      pure (App mtReturn typedFn typedArg')
    _ -> throwError UnknownTypeError -- can only apply onto function

inferIf ::
  Environment ->
  Annotation ->
  TcExpr ->
  TcExpr ->
  TcExpr ->
  InferM ElabExpr
inferIf env _ann ifExpr thenExpr elseExpr = do
  (typedIf, typedThen, typedElse) <-
    (,,)
      <$> infer env ifExpr
        <*> infer env thenExpr
        <*> infer env elseExpr
  subs <- unify (expAnn typedThen) (expAnn typedElse)
  let thenE = applySubst subs (expAnn typedThen)
  pure (If thenE typedIf typedThen typedElse)

inferConstructor ::
  Environment ->
  Annotation ->
  TyCon ->
  InferM ElabExpr
inferConstructor env ann tyCon = do
  mt <- inferDataConstructor env ann tyCon
  pure (Constructor mt tyCon)

infer :: Environment -> TcExpr -> InferM ElabExpr
infer env inferExpr =
  case inferExpr of
    (Lit ann a) -> inferLiteral ann a
    (Let ann binding bindExpr' inExpr) ->
      inferLet env ann binding bindExpr' inExpr
    (Var ann var) ->
      inferVarFromScope env ann var
    (Lambda ann binder body) ->
      inferLambda env ann binder body
    (Ann _ann mt expr) ->
      check env expr mt
    (App ann fn val) ->
      inferApp env ann fn val
    (If ann ifExpr thenExpr elseExpr) ->
      inferIf env ann ifExpr thenExpr elseExpr
    (Constructor ann tyCon) ->
      inferConstructor env ann tyCon

-- | This is actually application mode
inferLambda ::
  Environment ->
  Annotation ->
  Variable ->
  TcExpr ->
  InferM ElabExpr
inferLambda env ann binder body = do
  inputType <- popType
  combinedTyBinder <- case inputType of
    Just inputType' -> pure inputType'
    Nothing -> getUnknown ann
  let newEnv =
        envFromVar
          binder
          (Scheme [] combinedTyBinder)
          <> env
  typedBody <- infer newEnv body
  let mt = MTFunction ann combinedTyBinder (expAnn typedBody)
  pure (Lambda mt binder typedBody)

check :: Environment -> TcExpr -> MonoType -> InferM ElabExpr
check env expr mt = do
  typedExpr <- infer env expr
  subs <- unify (expAnn typedExpr) mt
  pure (applySubst (debugPretty "subs" subs) typedExpr)
