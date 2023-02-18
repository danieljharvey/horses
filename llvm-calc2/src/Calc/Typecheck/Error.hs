{-# LANGUAGE DerivingStrategies #-}
    {-# LANGUAGE OverloadedStrings #-}
      {-# LANGUAGE FlexibleContexts #-}
module Calc.Typecheck.Error (TypeError (..), typeErrorDiagnostic) where

import Calc.TypeUtils
import qualified Prettyprinter as PP
import Calc.SourceSpan
import Data.Maybe (catMaybes)
import Calc.Types.Type
import Data.Text (Text)
import qualified Data.Text as T
import Calc.Types.Annotation
import qualified Error.Diagnose as Diag
import qualified Prettyprinter.Render.Text as PP

data TypeError ann = PredicateIsNotBoolean ann (Type ann) |
    TypeMismatch (Type ann) (Type ann)
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
   in case e of
       (PredicateIsNotBoolean _ foundType) ->
          let report =
                Diag.Err
                  Nothing
                  ( prettyPrint $ "Predicate for an if statement should be a Boolean type, but instead found "
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
                  ["These two values should be of the same type"]
            in Diag.addReport diag report

       (TypeMismatch a b) ->
          let report =
                Diag.Err
                  Nothing
                  ( prettyPrint $ "Unification error! Expected matching types but found "
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
           in Diag.addReport diag report

renderWithWidth :: Int -> PP.Doc ann -> Text
renderWithWidth w doc = PP.renderStrict (PP.layoutPretty layoutOptions (PP.unAnnotate doc))
  where
    layoutOptions = PP.LayoutOptions {PP.layoutPageWidth = PP.AvailablePerLine w 1}


