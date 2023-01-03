{-# LANGUAGE FlexibleContexts #-}

module Language.Mimsa.Project.TypeSearch
  ( typeSearch,
    typeSearchFromText,
    FoundPath,
  )
where

import Control.Monad.Except
import Control.Monad.State
import Data.Bifunctor (first)
import Data.Either (isRight)
import Data.Functor
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Monoid
import Data.Text (Text)
import Language.Mimsa.Typechecker.NormaliseTypes
import Language.Mimsa.Typechecker.TcMonad
import Language.Mimsa.Typechecker.Unify
import Language.Mimsa.Core
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Typechecker
import Language.Mimsa.Types.Typechecker.Substitutions

normalise :: MonoType -> Type ()
normalise mt = normaliseType mt $> ()

typeSearch :: Map Name MonoType -> MonoType -> Map FoundPath MonoType
typeSearch items mt = M.filter (typeEquals mt) (typeMapToFoundPath items)

-- two types are Equal in this context if we can unify them together
typeEquals :: MonoType -> MonoType -> Bool
typeEquals needle mtB =
  if isSimple needle
    then isRight (unify' needle mtB)
    else normalise needle == normalise mtB

-- | isSimple == no vars
isSimple :: MonoType -> Bool
isSimple =
  getAll . isSimple'
  where
    isSimple' :: MonoType -> All
    isSimple' (MTVar _ _) = All False
    isSimple' (MTPrim _ _) = All True
    isSimple' MTConstructor {} = All True
    isSimple' other = withMonoidType isSimple' other

unify' :: MonoType -> MonoType -> Either TypeError Substitutions
unify' mtA mtB = runUnifyM (unify mtA mtB)

type UnifyM = ExceptT TypeError (State TypecheckState)

runUnifyM ::
  UnifyM a ->
  Either TypeError a
runUnifyM value =
  case either' of
    (Right a, _) -> Right a
    (Left e, _) -> Left e
  where
    either' =
      runState
        (runExceptT value)
        (defaultTcState mempty)

-- | given a type map, split it into paths
typeMapToFoundPath :: Map Name MonoType -> Map FoundPath MonoType
typeMapToFoundPath =
  M.fromList
    . ( ( splitRecords
            . (\(k, a) -> (FoundPath (NE.singleton k), a))
        )
          <=< M.toList
      )

splitRecords :: (FoundPath, MonoType) -> [(FoundPath, MonoType)]
splitRecords (path, mt) = case mt of
  MTRecord _ mtS _ ->
    let toSubPath (k, v) = (appendNameToFoundPath k path, v)
     in (splitRecords <$> toSubPath) =<< M.toList mtS
  _ -> [(path, mt)]

typeSearchFromText ::
  Map Name MonoType ->
  Text ->
  Either (Error Annotation) (Map FoundPath MonoType)
typeSearchFromText typeMap input = do
  mt <- first (ParseError input) (parseMonoType input)
  let found = typeSearch typeMap mt
  pure (normaliseType <$> found)
