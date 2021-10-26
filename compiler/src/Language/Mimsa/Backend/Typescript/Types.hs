{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Mimsa.Backend.Typescript.Types
  ( TSGeneric (..),
    TSType (..),
    TSConstructor (..),
    TSDataType (..),
    TSStringPart (..),
    TSLiteral (..),
    TSPattern (..),
    TSSpread (..),
    TSExpr (..),
    TSLetBody (..),
    TSArrayPart (..),
    TSBody (..),
    TSStatement (..),
    TSFunctionBody (..),
    TSOp (..),
    TSModule (..),
  )
where

import Data.Map (Map)
import Data.Set (Set)
import Data.Text (Text)
import Language.Mimsa.Types.Identifiers.Name
import Language.Mimsa.Types.Identifiers.TyCon

-- | which generics have been used already?
newtype TSGeneric = TSGeneric Text
  deriving newtype (Eq, Ord, Show)

data TSType
  = TSType (Maybe Text) Text [TSType] -- namespace, typeName, inner types
  | TSTypeVar Text
  | TSTypeFun Text TSType TSType
  | TSTypeArray TSType
  deriving stock (Eq, Ord, Show)

data TSConstructor = TSConstructor TyCon [TSType]
  deriving stock (Eq, Ord, Show)

data TSDataType = TSDataType TyCon [Text] [TSConstructor]
  deriving stock (Eq, Ord, Show)

data TSLiteral = TSBool Bool | TSString Text | TSInt Int
  deriving stock (Eq, Ord, Show)

data TSSpread
  = TSNoSpread
  | TSSpreadWildcard
  | TSSpreadValue Name
  deriving stock (Eq, Ord, Show)

data TSStringPart
  = TSStringVar Name
  | TSStringWildcard
  deriving stock (Eq, Ord, Show)

data TSPattern
  = TSPatternVar Name
  | TSPatternPair TSPattern TSPattern
  | TSPatternRecord (Map Name TSPattern)
  | TSPatternConstructor TyCon [TSPattern]
  | TSPatternLit TSLiteral
  | TSPatternArray [TSPattern] TSSpread
  | TSPatternWildcard
  | TSPatternString TSStringPart TSStringPart
  deriving stock (Eq, Ord, Show)

newtype TSLetBody = TSLetBody TSBody
  deriving newtype (Eq, Ord, Show)

data TSStatement
  = TSAssignment TSExpr (Maybe TSType) TSLetBody -- match pattern, type, body
  | TSConditional TSExpr TSLetBody -- pattern to match, body
  deriving stock (Eq, Ord, Show)

-- this could be top level or in a function body, it's a list of
-- assignments followed by either the return or an export
-- won't be prettyprinted directly as it depends on context
data TSBody = TSBody [TSStatement] TSExpr
  deriving stock (Eq, Ord, Show)

newtype TSFunctionBody = TSFunctionBody TSBody
  deriving newtype (Eq, Ord, Show)

data TSOp
  = TSEquals
  | TSAdd
  | TSSubtract
  | TSGreaterThanOrEqualTo
  | TSAnd
  | TSStringConcat
  deriving stock (Eq, Ord, Show)

data TSArrayPart
  = TSArrayItem TSExpr
  | TSArraySpread TSExpr
  deriving stock (Eq, Ord, Show)

data TSExpr
  = TSLit TSLiteral
  | TSFunction Name (Set TSGeneric) TSType (Maybe TSType) TSFunctionBody -- argName, generics, argType, returnType, body
  | TSRecord (Map Name TSExpr)
  | TSRecordAccess Name TSExpr
  | TSArray [TSArrayPart]
  | TSArrayAccess Int TSExpr
  | TSVar Name
  | TSApp TSExpr TSExpr
  | TSInfix TSOp TSExpr TSExpr
  | TSTernary TSExpr TSExpr TSExpr
  | TSData Text [TSExpr]
  | TSError Text
  | TSUnderscore
  deriving stock (Eq, Ord, Show)

data TSModule = TSModule [TSDataType] TSBody
  deriving stock (Eq, Ord, Show)