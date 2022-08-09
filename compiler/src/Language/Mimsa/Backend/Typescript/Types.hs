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
    TSName (..),
    TSImport (..),
  )
where

import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import Language.Mimsa.Printer
import Language.Mimsa.Types.Identifiers.TyCon
import Language.Mimsa.Types.Identifiers.TypeName

data TSImport
  = TSImportValue Text
  | TSImportType Text
  deriving stock (Eq, Ord, Show)

instance Printer TSImport where
  prettyPrint (TSImportType t) = t
  prettyPrint (TSImportValue t) = t

-- | which generics have been used already?
newtype TSGeneric = TSGeneric Text
  deriving newtype (Eq, Ord, Show, Printer)

data TSType
  = TSType (Maybe Text) Text [TSType] -- namespace, typeName, inner types
  | TSTypeVar Text
  | TSTypeFun Text TSType TSType
  | TSTypeArray TSType
  | TSTypeTuple [TSType]
  | TSTypeRecord (Map Text TSType)
  | TSTypeAnd TSType TSType
  deriving stock (Eq, Ord, Show)

data TSConstructor = TSConstructor TyCon [TSType]
  deriving stock (Eq, Ord, Show)

data TSDataType = TSDataType TypeName [Text] [TSConstructor]
  deriving stock (Eq, Ord, Show)

data TSLiteral = TSBool Bool | TSString Text | TSInt Int
  deriving stock (Eq, Ord, Show)

data TSSpread
  = TSNoSpread
  | TSSpreadWildcard
  | TSSpreadValue TSName
  deriving stock (Eq, Ord, Show)

data TSStringPart
  = TSStringVar TSName
  | TSStringWildcard
  deriving stock (Eq, Ord, Show)

data TSPattern
  = TSPatternVar TSName
  | TSPatternPair TSPattern TSPattern
  | TSPatternRecord (Map TSName TSPattern)
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
  | TSGreaterThan
  | TSGreaterThanOrEqualTo
  | TSLessThan
  | TSLessThanOrEqualTo
  | TSAnd
  | TSStringConcat
  deriving stock (Eq, Ord, Show)

data TSArrayPart
  = TSArrayItem TSExpr
  | TSArraySpread TSExpr
  deriving stock (Eq, Ord, Show)

newtype TSName = TSName Text
  deriving stock (Eq, Ord, Show)

instance IsString TSName where
  fromString = TSName . T.pack

data TSExpr
  = TSLit TSLiteral
  | TSFunction TSName (Set TSGeneric) TSType (Maybe TSType) TSFunctionBody -- argName, generics, argType, returnType, body
  | TSRecord (Map TSName TSExpr)
  | TSRecordAccess TSName TSExpr
  | TSArray [TSArrayPart]
  | TSArrayAccess Int TSExpr
  | TSVar TSName
  | TSApp TSExpr TSExpr
  | TSInfix TSOp TSExpr TSExpr
  | TSTernary TSExpr TSExpr TSExpr
  | TSData Text [TSExpr]
  | TSError Text
  | TSUnderscore
  deriving stock (Eq, Ord, Show)

data TSModule = TSModule [TSDataType] TSBody
  deriving stock (Eq, Ord, Show)
