{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Mimsa.Types.Bindings where

import qualified Data.Aeson as JSON
import Data.Map (Map)
import Language.Mimsa.Types.ExprHash
import Language.Mimsa.Types.Name

-- a list of names to hashes
newtype Bindings = Bindings {getBindings :: Map Name ExprHash}
  deriving newtype
    ( Eq,
      Ord,
      Show,
      Semigroup,
      Monoid,
      JSON.FromJSON,
      JSON.ToJSON
    )
