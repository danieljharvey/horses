{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Smol.Core.Typecheck.Errors
  ( typeErrorDiagnostic,
  )
where

import Data.Maybe (catMaybes, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Error.Diagnose
import Smol.Core.Printer
import Smol.Core.SourceSpan (sourceSpan)
import qualified Smol.Core.Typecheck.Shared as Smol
import Smol.Core.Typecheck.Types
import qualified Smol.Core.Types as Smol
import Smol.Core.Types.SourceSpan

-- use the derived Foldable instance to get all annotations in an error
getAllAnnotations :: TCError ann -> [ann]
getAllAnnotations = foldMap pure

positionFromAnnotation ::
  String ->
  Text ->
  Smol.Annotation ->
  Maybe Position
positionFromAnnotation path input ann =
  let toPos ss =
        Position
          (ssRowStart ss, ssColStart ss)
          (ssRowEnd ss, ssColEnd ss)
          path
   in toPos <$> sourceSpan input ann

prettyPrint :: (Printer a) => a -> Text
prettyPrint = renderWithWidth 40 . prettyDoc

typeErrorDiagnostic ::
  Text ->
  TCError Smol.Annotation ->
  Diagnostic Text
typeErrorDiagnostic input e =
  addReport diag report
  where
    filename = "<repl>"
    diag = addFile mempty filename (T.unpack input)
    report = case e of
      (TCExpectedFunction ty) ->
        Err
          Nothing
          ( "Expected a function type but got "
              <> prettyPrint ty
              <> "."
          )
          ( catMaybes
              [ (,)
                  <$> positionFromAnnotation
                    filename
                    input
                    (Smol.getTypeAnnotation ty)
                  <*> pure
                    ( This "Expected a function"
                    )
              ]
          )
          []
      (TCExpectedTuple ty) ->
        Err
          Nothing
          ( "Expected a tuple type but got "
              <> prettyPrint ty
              <> "."
          )
          ( catMaybes
              [ (,)
                  <$> positionFromAnnotation
                    filename
                    input
                    (Smol.getTypeAnnotation ty)
                  <*> pure
                    ( This "Expected a tuple"
                    )
              ]
          )
          []
      (TCExpectedRecord ty) ->
        Err
          Nothing
          ( "Expected a record type but got "
              <> prettyPrint ty
              <> "."
          )
          ( catMaybes
              [ (,)
                  <$> positionFromAnnotation
                    filename
                    input
                    (Smol.getTypeAnnotation ty)
                  <*> pure
                    ( This "Expected a record"
                    )
              ]
          )
          []
      (TCCouldNotFindVar ann var) ->
        Err
          Nothing
          ( "Could not find "
              <> prettyPrint var
              <> "."
          )
          ( catMaybes
              [ (,)
                  <$> positionFromAnnotation
                    filename
                    input
                    ann
                  <*> pure
                    ( This "Not found"
                    )
              ]
          )
          []
      (TCTypeMismatch a b) ->
        Err
          Nothing
          ( "Unification error! Expected matching types but found "
              <> prettyPrint a
              <> " and "
              <> prettyPrint b
              <> "."
          )
          ( catMaybes
              [ (,)
                  <$> positionFromAnnotation
                    filename
                    input
                    (Smol.getTypeAnnotation a)
                  <*> pure
                    ( This ("This has type " <> prettyPrint a)
                    ),
                (,)
                  <$> positionFromAnnotation
                    filename
                    input
                    (Smol.getTypeAnnotation b)
                  <*> pure (Where ("This has type " <> prettyPrint b))
              ]
          )
          ["These two values should be of the same type"]
      other ->
        let positions =
              mapMaybe
                (positionFromAnnotation filename input)
                (getAllAnnotations other)
         in Err
              Nothing
              (T.pack (show other))
              ((,Where "") <$> positions)
              []
