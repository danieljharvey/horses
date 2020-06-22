{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Typechecker.Infer
  ( startInference,
    doInference,
  )
where

import Control.Applicative
import Control.Monad.Except
import Control.Monad.Trans.State.Lazy
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Debug.Trace
import Language.Mimsa.Library
import Language.Mimsa.Syntax
import Language.Mimsa.Types

type App = StateT Substitutions (Except Text)

type Environment = M.Map Name MonoType

startInference :: Expr -> Either Text MonoType
startInference expr = doInference M.empty expr

doInference :: Environment -> Expr -> Either Text MonoType
doInference env expr =
  (fst <$> either')
  where
    either' = runExcept $ runStateT (infer env expr) M.empty

inferLiteral :: Literal -> App MonoType
inferLiteral (MyInt _) = pure MTInt
inferLiteral (MyBool _) = pure MTBool
inferLiteral (MyString _) = pure MTString
inferLiteral (MyUnit) = pure MTUnit

inferBuiltIn :: Name -> App MonoType
inferBuiltIn name = case getLibraryFunction name of
  Just ff -> pure $ getFFType ff
  _ -> throwError $ "Could not find built-in function " <> prettyPrint name

inferVarFromScope :: Environment -> Name -> App MonoType
inferVarFromScope env name = case M.lookup name env of
  Just a -> pure a
  _ -> throwError $ T.pack ("Unknown variable " <> show name)

infer :: Environment -> Expr -> App MonoType
infer _ (MyLiteral a) = inferLiteral a
infer env (MyVar name) = (inferVarFromScope env name) <|> (inferBuiltIn name)
infer env (MyLet binder expr body) = do
  tyExpr <- infer env expr
  let newEnv = M.insert binder tyExpr env
  infer newEnv body
infer env (MyLetPair binder1 binder2 expr body) = do
  tyExpr <- infer env expr
  case tyExpr of
    (MTPair a b) -> do
      _ <- unify tyExpr (MTPair a b)
      let newEnv = M.insert binder1 a (M.insert binder2 b env)
      infer newEnv body
    (MTUnknown i) -> do
      unknownA <- getUnknown
      unknownB <- getUnknown
      _ <- modify (\s -> traceShowId s)
      let matches =
            (\name -> (name, MTPair unknownA unknownB))
              <$> findByUniVar env i
      let newEnv =
            ( M.fromList
                [ (binder1, unknownA),
                  (binder2, unknownB)
                ]
            )
              <> M.fromList matches
              <> env
      infer (traceShowId newEnv) body
    a -> throwError $ "Expected a pair but instead found " <> prettyPrint a
infer env (MyLambda binder body) = do
  tyArg <- getUnknown
  let newEnv = M.insert binder tyArg env
  tyBody <- infer newEnv body
  pure $ MTFunction tyArg tyBody
infer env (MyApp function argument) = do
  tyArg <- infer env argument
  tyFun <- infer env function
  tyRes <- getUnknown
  -- tyFun = tyArg -> tyRes
  _ <- unify tyFun (MTFunction tyArg tyRes)
  apply tyRes
infer env (MyIf condition thenCase elseCase) = do
  tyCond <- infer env condition
  tyThen <- infer env thenCase
  tyElse <- infer env elseCase
  _ <- unify tyCond MTBool
  _ <- unify tyThen tyElse
  pure tyThen
infer env (MyPair a b) = do
  tyA <- infer env a
  tyB <- infer env b
  pure (MTPair tyA tyB)

findByUniVar :: Environment -> UniVar -> [Name]
findByUniVar env i = M.keys $ M.filter ((==) (MTUnknown i)) env

getUniVars :: MonoType -> [UniVar] -> [UniVar]
getUniVars (MTFunction argument result) as = (getUniVars argument as) ++ (getUniVars result as)
getUniVars (MTUnknown a) as = [a] ++ as
getUniVars _ as = as

unknowns :: MonoType -> [UniVar]
unknowns mType = getUniVars mType []

unify :: MonoType -> MonoType -> App ()
unify ty1' ty2' = do
  ty1 <- apply ty1'
  ty2 <- apply ty2'
  unify' ty1 ty2

unify' :: MonoType -> MonoType -> App ()
unify' a b | a == b = pure ()
unify' (MTFunction args result) (MTFunction args' result') = do
  unify args args'
  unify result result'
unify' (MTUnknown i) b = do
  occursCheck i b
  unifyVariable i b
unify' a (MTUnknown i) = do
  occursCheck i a
  unifyVariable i a
unify' (MTPair a b) (MTPair a' b') = do
  unify a a'
  unify b b'
unify' a b =
  throwError $ T.pack $
    "Can't match " <> show a <> " with " <> show b

occursCheck :: UniVar -> MonoType -> App ()
occursCheck i mt =
  if (not $ elem i (unknowns mt))
    then pure ()
    else
      throwError $ T.pack $
        "Cannot unify as " <> show (MTUnknown i) <> " occurs within " <> show mt

-- all the Ints we've matched back to types
type Substitutions = M.Map UniVar (Maybe MonoType)

-- replace unknowns with knowns from the substitution list where possible
apply :: MonoType -> App MonoType
apply (MTUnknown i) = do
  sub <- join <$> gets (M.lookup i)
  case sub of
    Just mType -> (apply mType)
    Nothing -> pure (MTUnknown i)
apply (MTFunction args result) =
  MTFunction <$> (apply args) <*> (apply result)
apply (MTPair a b) = MTPair <$> (apply a) <*> (apply b)
apply other = pure other

getUnknown :: App MonoType
getUnknown = do
  nextUniVar <- gets (\subs -> (M.foldlWithKey (\k k' _ -> max k k') 0 subs) + 1)
  modify (M.insert nextUniVar Nothing)
  pure (MTUnknown nextUniVar)

unifyVariable :: UniVar -> MonoType -> App ()
unifyVariable i mType = modify (M.insert i (Just mType))
