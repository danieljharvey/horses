{-# LANGUAGE DerivingStrategies #-}

module Language.Mimsa.Backend.Types
  ( BackendM,
    Backend (..),
  )
where

import Language.Mimsa.Types.Error

type BackendM ann = Either (BackendError ann)

data Backend = ESModulesJS | Typescript
  deriving stock (Eq, Ord, Show)
