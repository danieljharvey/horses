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
    withFunction,
  )
where

import Calc.Typecheck.Error
import Calc.Types.Function
import Calc.Types.Identifier
import Calc.Types.Type
import Control.Monad.Except
import Control.Monad.Reader
import Data.Bifunctor (first)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM

data TypecheckEnv ann = TypecheckEnv
  { tceVars :: HashMap Identifier (Type ann),
    tceFunctions :: HashMap FunctionName (Type ann)
  }

newtype TypecheckM ann a = TypecheckM
  { getTypecheckM ::
      ReaderT (TypecheckEnv ann) (Either (TypeError ann)) a
  }
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadReader (TypecheckEnv ann),
      MonadError (TypeError ann)
    )

runTypecheckM ::
  TypecheckEnv ann ->
  TypecheckM ann a ->
  Either (TypeError ann) a
runTypecheckM env action =
  runReaderT (getTypecheckM action) env

withFunction :: FunctionName -> Type ann -> TypecheckM ann a -> TypecheckM ann a
withFunction fnName ty =
  local
    ( \tce ->
        tce
          { tceFunctions =
              HM.insert fnName ty (tceFunctions tce)
          }
    )

-- | look up a saved identifier "in the environment"
lookupFunction :: ann -> FunctionName -> TypecheckM ann (Type ann)
lookupFunction ann fnName = do
  maybeType <- asks (HM.lookup fnName . tceFunctions)
  case maybeType of
    Just found -> pure found
    Nothing -> do
      allFunctions <- asks (HM.keysSet . tceFunctions)
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
