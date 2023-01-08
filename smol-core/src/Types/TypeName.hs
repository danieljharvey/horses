{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Types.TypeName
  ( TypeName (..),
  )
where

import Data.String
import qualified Data.Text as T
import Printer
import Types.Constructor

newtype TypeName = TypeName Constructor
  deriving newtype (Eq, Ord, Show)

instance Printer TypeName where
  prettyDoc (TypeName n) = prettyDoc n

instance IsString TypeName where
  fromString = TypeName . Constructor . T.pack
