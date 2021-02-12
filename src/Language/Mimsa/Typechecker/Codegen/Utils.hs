{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Typechecker.Codegen.Utils
  ( nextName,
    CodegenM,
    runCodegenM,
    getFunctorVar,
    getMapItems,
  )
where

import Control.Monad.Except
import Control.Monad.State
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as M
import Data.Semigroup
import Data.Text (Text)
import Language.Mimsa.Printer
import Language.Mimsa.Types.Identifiers
import Prelude hiding (fmap)

type CodegenM = StateT (Map Name Int) (Either Text)

runCodegenM :: CodegenM a -> Either Text a
runCodegenM fn = case runStateT fn mempty of
  Right (a, _) -> pure a
  Left e -> Left e

-- get last type variable
getFunctorVar :: [Name] -> CodegenM Name
getFunctorVar names = case NE.nonEmpty names of
  Just neNames -> pure $ NE.last neNames
  _ -> throwError "Type should have at least one type variable"

-- | given a type constructor, give me a new unique name for it
nextName :: TyCon -> CodegenM Name
nextName tyCon = do
  let base = tyConToName tyCon
  vars <- get
  case M.lookup base vars of
    Nothing -> do
      modify (M.singleton base 1 <>)
      pure $ base <> Name "1"
    Just as -> do
      modify (M.adjust (+ 1) base)
      pure $ base <> Name (prettyPrint (as + 1))

getMapItems :: Map k a -> Maybe (NE.NonEmpty (k, a))
getMapItems = NE.nonEmpty . M.toList
