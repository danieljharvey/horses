{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types.MonoType
  ( MonoType (..),
  )
where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Language.Mimsa.Types.Construct
import Language.Mimsa.Types.Name
import Language.Mimsa.Types.Printer
import Language.Mimsa.Types.Variable

-------------

data MonoType
  = MTInt
  | MTString
  | MTBool
  | MTUnit
  | MTFunction MonoType MonoType -- argument, result
  | MTPair MonoType MonoType -- (a,b)
  | MTSum MonoType MonoType -- a | b
  | MTList MonoType -- [a]
  | MTRecord (Map Name MonoType) -- { foo: a, bar: b }
  | MTVar Variable
  | MTData Construct [MonoType] -- name, typeVars
  deriving (Eq, Ord, Show)

-----------

inParens :: (Printer a) => a -> Text
inParens a = "(" <> prettyPrint a <> ")"

instance Printer MonoType where
  prettyPrint MTInt = "Int"
  prettyPrint MTString = "String"
  prettyPrint MTBool = "Boolean"
  prettyPrint MTUnit = "Unit"
  prettyPrint (MTData consName tyVars) =
    prettyPrint consName <> " "
      <> T.intercalate " " (prettyPrint <$> tyVars)
  prettyPrint (MTFunction a b) = printSubType a <> " -> " <> printSubType b
  prettyPrint (MTPair a b) = "(" <> printSubType a <> ", " <> printSubType b <> ")"
  prettyPrint (MTVar a) = prettyPrint a
  prettyPrint (MTSum a b) = "Sum " <> printSubType a <> " " <> printSubType b
  prettyPrint (MTList a) = "List " <> printSubType a
  prettyPrint (MTRecord as) = "{" <> T.intercalate ", " types <> "}"
    where
      types =
        ( \(name, mt) ->
            prettyPrint name
              <> ": "
              <> printSubType mt
        )
          <$> M.toList as

-- simple things with no brackets, complex things in brackets
printSubType :: MonoType -> Text
printSubType all'@(MTSum _ _) = inParens all'
printSubType all'@(MTFunction _ _) = inParens all'
printSubType all'@(MTList _) = inParens all'
printSubType a = prettyPrint a
