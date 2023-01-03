{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Language.Mimsa.Core.Types.Identifiers.TestName
  (

    TestName (..),






  )
where

import qualified Data.Aeson as JSON
import Data.OpenApi
import Data.Text (Text)
import Language.Mimsa.Core.Printer

newtype TestName = TestName Text
  deriving newtype
    ( Eq,
      Ord,
      Show,
      JSON.ToJSON,
      JSON.FromJSON,
      ToSchema
    )

instance Printer TestName where
  prettyPrint (TestName n) = n


