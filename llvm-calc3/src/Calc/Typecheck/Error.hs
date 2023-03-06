{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Calc.Typecheck.Error (TypeError (..), typeErrorDiagnostic) where

import Calc.SourceSpan
import Calc.TypeUtils
import Calc.Types.Annotation
import Calc.Types.Expr
import Calc.Types.Type
import Data.Maybe (catMaybes, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Error.Diagnose as Diag
import qualified Prettyprinter as PP
import qualified Prettyprinter.Render.Text as PP

data TypeError ann
  = PredicateIsNotBoolean ann (Type ann)
  | InfixTypeMismatch Op [(Type ann, Type ann)]
  | TypeMismatch (Type ann) (Type ann)
  deriving stock (Eq, Ord, Show)

positionFromAnnotation ::
  String ->
  Text ->
  Annotation ->
  Maybe Diag.Position
positionFromAnnotation path input ann =
  let toPos ss =
        Diag.Position
          (ssRowStart ss, ssColStart ss)
          (ssRowEnd ss, ssColEnd ss)
          path
   in toPos <$> sourceSpan input ann

prettyPrint :: PP.Doc doc -> Text
prettyPrint = renderWithWidth 40

typeErrorDiagnostic ::
  Text ->
  TypeError Annotation ->
  Diag.Diagnostic Text
typeErrorDiagnostic input e =
  let filename = "<repl>"
      diag = Diag.addFile Diag.def filename (T.unpack input)
      report = case e of
        (PredicateIsNotBoolean _ foundType) ->
          Diag.Err
            Nothing
            ( prettyPrint $
                "Predicate for an if statement should be a Boolean type, but instead found "
                  <> PP.pretty foundType
                  <> "."
            )
            ( catMaybes
                [ (,)
                    <$> positionFromAnnotation
                      filename
                      input
                      (getOuterTypeAnnotation foundType)
                    <*> pure
                      ( Diag.This (prettyPrint $ "This has type " <> PP.pretty foundType <> " but should have type Boolean")
                      )
                ]
            )
            []
        (TypeMismatch a b) ->
          Diag.Err
            Nothing
            ( prettyPrint $
                "Unification error! Expected matching types but found "
                  <> PP.pretty a
                  <> " and "
                  <> PP.pretty b
                  <> "."
            )
            ( catMaybes
                [ (,)
                    <$> positionFromAnnotation
                      filename
                      input
                      (getOuterTypeAnnotation a)
                    <*> pure
                      ( Diag.This (prettyPrint $ "This has type " <> PP.pretty a)
                      ),
                  (,)
                    <$> positionFromAnnotation
                      filename
                      input
                      (getOuterTypeAnnotation b)
                    <*> pure (Diag.Where (prettyPrint $ "This has type " <> PP.pretty b))
                ]
            )
            ["These two values should be of the same type"]
        (InfixTypeMismatch _op pairs) ->
          let makeThis (expect, actual) =
                (,)
                  <$> positionFromAnnotation
                    filename
                    input
                    (getOuterTypeAnnotation actual)
                  <*> pure
                    ( Diag.This (prettyPrint $ "This has type " <> PP.pretty actual <> " but should have type " <> PP.pretty expect)
                    )
           in Diag.Err
                Nothing
                "Type mismatch for infix operator"
                ( mapMaybe makeThis pairs
                )
                []
   in Diag.addReport diag report

renderWithWidth :: Int -> PP.Doc ann -> Text
renderWithWidth w doc = PP.renderStrict (PP.layoutPretty layoutOptions (PP.unAnnotate doc))
  where
    layoutOptions = PP.LayoutOptions {PP.layoutPageWidth = PP.AvailablePerLine w 1}
