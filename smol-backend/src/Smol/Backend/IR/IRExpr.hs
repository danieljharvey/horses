{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Smol.Backend.IR.IRExpr
  ( IRModulePart (..),
    IRExtern (..),
    IRModule (..),
    IRFunction (..),
    IRIdentifier (..),
    IRPrim (..),
    IRType (..),
    IRFunctionName (..),
    IRState (..),
    IROp (..),
    IRExpr (..),
    IRSetTo (..),
    IRStatement (..),
    IRMatchCase (..),
    prettyModule,
  )
where

import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word64)
import qualified LLVM.AST as LLVM hiding (function)
import Smol.Backend.Types.GetPath
import Smol.Backend.Types.PatternPredicate

newtype IRName = IRName String
  deriving newtype (Eq, Ord, Show)

newtype IRFunctionName = IRFunctionName String
  deriving newtype (Eq, Ord, Show)

instance IsString IRFunctionName where
  fromString = IRFunctionName

data IRType
  = IRInt2
  | IRInt8
  | IRInt32
  | IRStruct [IRType]
  | IRPointer IRType
  | IRFunctionType [IRType] IRType
  | IRArray Word64 IRType
  deriving stock (Eq, Ord, Show)

data IRPrim
  = IRPrimInt2 Bool
  | IRPrimInt32 Integer
  deriving stock (Eq, Ord, Show)

newtype IRModule = IRModule [IRModulePart]
  deriving newtype (Eq, Ord, Show)

newtype IRIdentifier = IRIdentifier String
  deriving newtype (Eq, Ord, Show)

instance IsString IRIdentifier where
  fromString = IRIdentifier

data IROp = IRAdd | IREquals
  deriving stock (Eq, Ord, Show)

data IRModulePart
  = IRExternDef IRExtern
  | IRFunctionDef IRFunction
  deriving stock (Eq, Ord, Show)

data IRStatement
  = IRSet [Integer] IRType IRExpr IRExpr -- (path, fromType, from, to)  set part of a struct to a value
  | IRDiscard IRExpr -- do the thing, bin the result
  | IRRet IRType IRExpr
  deriving stock (Eq, Ord, Show)

data IRSetTo = IRSetTo
  { irstPath :: [Integer],
    irstType :: IRType,
    irstExpr :: IRExpr
  }
  deriving stock (Eq, Ord, Show)

data IRExpr
  = IRAlloc IRType
  | IRPrim IRPrim
  | IRInfix IROp IRExpr IRExpr
  | IRApply IRType IRExpr [IRExpr] -- fnType, fn, [args]
  | IRVar IRIdentifier
  | IRLet IRIdentifier IRExpr IRExpr
  | IRStructPath [Integer] IRExpr
  | IRFuncPointer IRFunctionName -- this will get turned into a lookup for a given function by name
  | IRMatch IRExpr IRType (NE.NonEmpty IRMatchCase)
  | IRStatements [IRStatement] IRExpr -- [things to do], value
  | IRPointerTo [Integer] IRExpr
  | IRCast IRType IRExpr
  | IRInitialiseDataType IRExpr IRType IRType [IRSetTo] -- where to put stuff, type of whole thing, type of constructor, values
  | IRString Text
  deriving stock (Eq, Ord, Show)

data IRMatchCase = IRMatchCase
  { irmcType :: IRType,
    irmcPatternPredicate :: [PatternPredicate IRExpr],
    irmcGetPath :: Map IRIdentifier GetPath,
    irmcExpr :: IRExpr
  }
  deriving stock (Eq, Ord, Show)

-- a top level function
data IRFunction = IRFunction
  { irfName :: IRFunctionName,
    irfArgs :: [(IRType, IRIdentifier)],
    irfReturn :: IRType,
    irfBody :: [IRStatement]
  }
  deriving stock (Eq, Ord, Show)

data IRExtern = IRExtern
  { ireName :: IRFunctionName,
    ireArgs :: [IRType],
    ireReturn :: IRType
  }
  deriving stock (Eq, Ord, Show)

data IRState = IRState
  { irFunctions :: Map IRFunctionName (Either IRFunction IRExtern),
    irVars :: Map IRIdentifier LLVM.Operand,
    irStrings :: Map Text LLVM.Operand
  }

prettyModule :: IRModule -> Text
prettyModule (IRModule parts) =
  T.intercalate "\n\n" (prettyModulePart <$> parts)

prettyModulePart :: IRModulePart -> Text
prettyModulePart = T.pack . show
