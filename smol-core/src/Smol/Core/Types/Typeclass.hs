{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Smol.Core.Types.Typeclass
  ( TypeclassName (..),
    Pred (..),
    Qual (..),
    Typeclass,
    Instance,
    TypeclassEnv (..),
    TypeclassError (..),
    addTypeclass,
    getSuperclass,
    getInstances,
    insertTypeclassInstance,
    builtInTypeclasses,
    addInstance,
    bySuper
  )
where

import Control.Monad (unless, when)
import Control.Monad.Except
import Control.Monad.Identity
import Data.Foldable (traverse_)
import qualified Data.Map.Strict as M
import Data.String
import qualified Data.Text as T
import Smol.Core.Types.Type
import Smol.Core.Types.TypeName

newtype TypeclassName = TypeclassName T.Text
  deriving newtype (Eq, Ord, Show)

instance IsString TypeclassName where
  fromString = TypeclassName . fromString

data Pred = IsIn TypeclassName (Type Identity ())
  deriving stock (Eq, Ord, Show)

data Qual t = Qualified [Pred] t
  deriving stock (Eq, Ord, Show)

data Typeclass = Typeclass [TypeclassName] [Instance]
  deriving stock (Eq, Ord, Show)

type Instance = Qual Pred

data TypeclassEnv = TypeclassEnv
  { tceClasses :: M.Map TypeclassName Typeclass,
    tceDefaults :: [TypeName]
  }
  deriving stock (Eq, Ord, Show)

instance Semigroup TypeclassEnv where
  TypeclassEnv a b <> TypeclassEnv a' b' =
    TypeclassEnv (a <> a') (b <> b')

instance Monoid TypeclassEnv where
  mempty = TypeclassEnv mempty mempty

data TypeclassError
  = AlreadyDefined TypeclassName
  | SuperclassNotFound TypeclassName
  | NoClassFoundForInstance TypeclassName
  | OverlappingInstance
  deriving stock (Eq, Ord, Show)

getSuperclass :: TypeclassEnv -> TypeclassName -> [TypeclassName]
getSuperclass (TypeclassEnv {tceClasses}) tc =
  case M.lookup tc tceClasses of
    Just (Typeclass typeclassNames _) -> typeclassNames
    _ -> []

getInstances :: TypeclassEnv -> TypeclassName -> [Instance]
getInstances (TypeclassEnv {tceClasses}) tc =
  case M.lookup tc tceClasses of
    Just (Typeclass _ instances) -> instances
    _ -> []

insertTypeclassInstance :: TypeclassEnv -> TypeclassName -> Typeclass -> TypeclassEnv
insertTypeclassInstance (TypeclassEnv {tceClasses, tceDefaults}) name tc =
  TypeclassEnv
    { tceDefaults = tceDefaults,
      tceClasses = M.insert name tc tceClasses
    }

-- add a new typeclass
-- we check that a) it doesn't already exist
-- b) all its superclasses exist
addTypeclass ::
  TypeclassName ->
  [TypeclassName] ->
  TypeclassEnv ->
  Either TypeclassError TypeclassEnv
addTypeclass name instances (TypeclassEnv {tceDefaults, tceClasses}) = do
  when
    (M.member name tceClasses)
    (throwError (AlreadyDefined name))
  traverse_
    ( \superclassName ->
        unless
          (M.member superclassName tceClasses)
          (throwError (SuperclassNotFound superclassName))
    )
    instances

  pure
    ( TypeclassEnv
        { tceDefaults = tceDefaults,
          tceClasses =
            M.insert name (Typeclass instances []) tceClasses
        }
    )

builtInTypeclasses :: Either TypeclassError TypeclassEnv
builtInTypeclasses =
  pure mempty
    >>= addTypeclass "Eq" []
    >>= addTypeclass "Ord" ["Eq"]
    >>= addTypeclass "Show" []
    >>= addTypeclass "Bounded" []
    >>= addTypeclass "Enum" []

-- | are these basically the same?
-- we should check using `isSubtypeOf`
overlap :: Pred -> Pred -> Bool
overlap _ _ = False

-- | we do not check for overlapping yet
addInstance :: [Pred] -> Pred -> TypeclassEnv -> Either TypeclassError TypeclassEnv
addInstance preds pred'@(IsIn typeclassName _) tce
  | M.notMember typeclassName (tceClasses tce) = throwError (NoClassFoundForInstance typeclassName)
  | any (overlap pred') quals = throwError OverlappingInstance
  | otherwise = pure (insertTypeclassInstance tce typeclassName newTypeclass)
  where
    instances :: [Instance]
    instances = getInstances tce typeclassName

    quals :: [Pred]
    quals = (\(Qualified _ types) -> types) <$> instances

    newTypeclass :: Typeclass
    newTypeclass =
      Typeclass
        (getSuperclass tce typeclassName)
        (Qualified preds pred' : instances)

-- | given a Pred like `Ord Int`, this tells us we've also got `Eq Int`
-- implicitly because it was required 
bySuper :: TypeclassEnv -> Pred -> [Pred]
bySuper tce pred'@(IsIn typeclassName ty)
  = pred' : concatMap 
      (\superTypeclassName -> bySuper tce (IsIn superTypeclassName ty)) 
          (getSuperclass tce typeclassName)
