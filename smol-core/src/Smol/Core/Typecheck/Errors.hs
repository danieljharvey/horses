{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Smol.Core.Typecheck.Errors
  ( typeErrorDiagnostic,
  )
where
import Smol.Core.SourceSpan (sourceSpan)

import Data.Maybe ( catMaybes, mapMaybe )
import Data.Text (Text)
import qualified Data.Text as T
import Error.Diagnose
import Smol.Core.Printer
import Smol.Core.Types.SourceSpan
import qualified Smol.Core.Typecheck.Shared as Smol
import Smol.Core.Typecheck.Types
import qualified Smol.Core.Types as Smol

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
  let filename = "<repl>"
      diag = addFile def filename (T.unpack input)
   in case e of
        (TCTypeMismatch a b) ->
          let report =
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
           in addReport diag report
        other ->
          let positions =
                mapMaybe
                  (positionFromAnnotation filename input)
                  (getAllAnnotations other)
              report =
                Err
                  Nothing
                  (T.pack (show other))
                  ((,Where "") <$> positions)
                  []
           in addReport diag report
