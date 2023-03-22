{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Calc.Typecheck.Types
  ( TypecheckM (..),
    runTypecheckM,
    TypecheckEnv (..),
    lookupVar,
    withVar,
    lookupFunction,
    withFunctionArgs,
    storeFunction,
  )
where

import Calc.Typecheck.Error
import Calc.Types.Function
import Calc.Types.Identifier
import Calc.Types.Type
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Bifunctor (first)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM

newtype TypecheckEnv ann = TypecheckEnv
  { tceVars :: HashMap Identifier (Type ann)
  }
  deriving stock (Eq, Ord, Show)

newtype TypecheckState ann = TypecheckState
  {tcsFunctions :: HashMap FunctionName (Type ann)}
  deriving stock (Eq, Ord, Show)

newtype TypecheckM ann a = TypecheckM
  { getTypecheckM ::
      ReaderT (TypecheckEnv ann) (StateT (TypecheckState ann) (Either (TypeError ann))) a
  }
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadReader (TypecheckEnv ann),
      MonadError (TypeError ann),
      MonadState (TypecheckState ann)
    )

runTypecheckM ::
  TypecheckEnv ann ->
  TypecheckM ann a ->
  Either (TypeError ann) a
runTypecheckM env action =
  evalStateT (runReaderT (getTypecheckM action) env) (TypecheckState mempty)

storeFunction ::
  FunctionName ->
  Type ann ->
  TypecheckM ann ()
storeFunction fnName ty =
  modify
    ( \tcs ->
        tcs
          { tcsFunctions =
              HM.insert fnName ty (tcsFunctions tcs)
          }
    )

-- | look up a saved identifier "in the environment"
lookupFunction :: ann -> FunctionName -> TypecheckM ann (Type ann)
lookupFunction ann fnName = do
  maybeType <- gets (HM.lookup fnName . tcsFunctions)
  case maybeType of
    Just found -> pure found
    Nothing -> do
      allFunctions <- gets (HM.keysSet . tcsFunctions)
      throwError (FunctionNotFound ann fnName allFunctions)

-- | look up a saved identifier "in the environment"
lookupVar :: ann -> Identifier -> TypecheckM ann (Type ann)
lookupVar ann identifier = do
  maybeType <- asks (HM.lookup identifier . tceVars)
  case maybeType of
    Just found -> pure found
    Nothing -> do
      allIdentifiers <- asks (HM.keysSet . tceVars)
      throwError (VarNotFound ann identifier allIdentifiers)

-- | add an identifier to the environment
withVar :: Identifier -> Type ann -> TypecheckM ann a -> TypecheckM ann a
withVar identifier ty =
  local
    ( \tce ->
        tce
          { tceVars =
              HM.insert identifier ty (tceVars tce)
          }
    )

withFunctionArgs :: [(ArgumentName, Type ann)] -> TypecheckM ann a -> TypecheckM ann a
withFunctionArgs args =
  local
    ( \tce ->
        tce
          { tceVars = tceVars tce <> HM.fromList tidiedArgs
          }
    )
  where
    tidiedArgs =
      fmap (first (\(ArgumentName arg) -> Identifier arg)) args
