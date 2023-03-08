{-# LANGUAGE DerivingStrategies #-}
  {-# LANGUAGE GeneralisedNewtypeDeriving #-}
module Calc.Typecheck.Types (TypecheckM(..), runTypecheckM, TypecheckEnv, lookupVar, withVar, withFunctionArgs) where

import Data.Bifunctor (first)
import Calc.Types.Function
import qualified Data.HashMap.Strict as HM
import Control.Monad.Except
import Control.Monad.Reader
import Calc.Typecheck.Error
import Data.HashMap.Strict (HashMap)
import Calc.Types.Type
import Calc.Types.Identifier

type TypecheckEnv ann = HashMap Identifier (Type ann)

newtype TypecheckM ann a =
  TypecheckM { getTypecheckM ::
    ReaderT (TypecheckEnv ann) (Either (TypeError ann)) a }
    deriving newtype (Functor, Applicative, Monad, MonadReader (TypecheckEnv ann),
        MonadError (TypeError ann))

runTypecheckM :: TypecheckEnv ann -> TypecheckM ann a -> Either (TypeError ann) a
runTypecheckM env action
 = runReaderT (getTypecheckM action) env

-- | look up a saved identifier "in the environment"
lookupVar :: ann -> Identifier -> TypecheckM ann (Type ann)
lookupVar ann identifier = do
  maybeType <- asks (HM.lookup identifier)
  case maybeType of
    Just found -> pure found
    Nothing -> do
      allIdentifiers <- asks HM.keysSet
      throwError (VarNotFound ann identifier allIdentifiers)

-- | add an identifier to the environment
withVar :: Identifier -> Type ann -> TypecheckM ann a -> TypecheckM ann a
withVar identifier ty =
  local (HM.insert identifier ty)

withFunctionArgs :: [(ArgumentName, Type ann)] -> TypecheckM ann a -> TypecheckM ann a
withFunctionArgs args =
  local (\hm -> hm <> HM.fromList tidiedArgs)
    where
      tidiedArgs =
          fmap (first (\(ArgumentName arg) -> Identifier arg)) args

