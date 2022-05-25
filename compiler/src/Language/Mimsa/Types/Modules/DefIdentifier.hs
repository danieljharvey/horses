{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types.Modules.DefIdentifier
  ( 
   
    DefIdentifier (..),
  
 
  )
where

import qualified Data.Aeson as JSON
import GHC.Generics
import Language.Mimsa.Printer
import Language.Mimsa.Types.AST.InfixOp
import Language.Mimsa.Types.Identifiers

-- | what are we typechecking here?
data DefIdentifier = DIName Name | DIInfix InfixOp
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (JSON.ToJSON, JSON.ToJSONKey, 
      JSON.FromJSON, JSON.FromJSONKey)

instance Printer DefIdentifier where
  prettyPrint (DIName name) = prettyPrint name
  prettyPrint (DIInfix infixOp) = prettyPrint infixOp


