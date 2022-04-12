{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Language.Mimsa.Types.Store.RootPath
  ( RootPath (..),
  )
where

newtype RootPath = RootPath String
  deriving newtype (Eq, Ord, Show)
