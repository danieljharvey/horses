{-# LANGUAGE FlexibleContexts #-}

module Language.Mimsa.Project.TypeSearch
  ( typeSearch,
    typeSearchFromText,
    FoundPath,
  )
where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Bifunctor (first)
import Data.Either (isRight)
import Data.Functor
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import Language.Mimsa.Parser.Helpers (parseAndFormat)
import Language.Mimsa.Parser.MonoType
import Language.Mimsa.Typechecker.NormaliseTypes
import Language.Mimsa.Typechecker.TcMonad
import Language.Mimsa.Typechecker.Unify
import Language.Mimsa.Types.AST.Annotation
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Swaps
import Language.Mimsa.Types.Typechecker

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
isSimple (MTVar _ _) = False
isSimple (MTFunction _ a b) = isSimple a && isSimple b
isSimple (MTPrim _ _) = True
isSimple (MTPair _ a b) = isSimple a && isSimple b
isSimple (MTRecord _ as) = and (isSimple <$> as)
isSimple (MTRecordRow _ as b) =
  isSimple b
    && and (isSimple <$> as)
isSimple (MTArray _ as) = isSimple as
isSimple (MTConstructor _ _) = True
isSimple (MTTypeApp _ fn val) = isSimple fn && isSimple val

unify' :: MonoType -> MonoType -> Either TypeError Substitutions
unify' mtA mtB = runUnifyM mempty (unify mtA mtB)

type UnifyM = ExceptT TypeError (ReaderT Swaps (State TypecheckState))

runUnifyM ::
  Swaps ->
  UnifyM a ->
  Either TypeError a
runUnifyM swaps value =
  case either' of
    (Right a, _) -> Right a
    (Left e, _) -> Left e
  where
    either' =
      runState
        (runReaderT (runExceptT value) swaps)
        defaultTcState

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
  MTRecord _ mtS ->
    let toSubPath (k, v) = (appendNameToFoundPath k path, v)
     in (splitRecords <$> toSubPath) =<< M.toList mtS
  _ -> [(path, mt)]

typeSearchFromText ::
  Map Name MonoType ->
  Text ->
  Either (Error Annotation) (Map FoundPath MonoType)
typeSearchFromText typeMap input = do
  mt <- first ParseError (parseAndFormat monoTypeParser input)
  let found = typeSearch typeMap mt
  pure (normaliseType <$> found)
