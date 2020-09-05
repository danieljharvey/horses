{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Language.Mimsa.Types.Literal
  ( Literal (..),
  )
where

import qualified Data.Aeson as JSON
import GHC.Generics
import Language.Mimsa.Types.StringType

-------

data Literal
  = MyInt Int
  | MyBool Bool
  | MyString StringType
  | MyUnit
  deriving
    ( Eq,
      Ord,
      Show,
      Generic,
      JSON.FromJSON,
      JSON.ToJSON
    )
