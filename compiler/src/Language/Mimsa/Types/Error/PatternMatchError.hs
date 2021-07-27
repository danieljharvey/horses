{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types.Error.PatternMatchError
  ( PatternMatchError (..),
    renderPatternMatchError,
    getErrorPos,
  )
where

import Data.Set (Set)
import qualified Data.Text as T
import Data.Text.Prettyprint.Doc
import Language.Mimsa.Printer
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Text.Megaparsec

data PatternMatchError
  = -- | No patterns provided
    EmptyPatternMatch Annotation
  | -- | "Just 1 2" or "Nothing 3", for instance
    -- | ann, offending tyCon, expected, actual
    ConstructorArgumentLengthMismatch Annotation TyCon Int Int
  | -- | Cases not covered in pattern matches
    -- | ann, [missing patterns]
    MissingPatterns Annotation [Pattern Variable Annotation]
  | -- | Unnecessary cases covered by previous matches
    RedundantPatterns Annotation [Pattern Variable Annotation]
  | -- | Multiple instances of the same variable
    DuplicateVariableUse Annotation (Set Name)
  deriving stock (Eq, Ord, Show)

------

instance Semigroup PatternMatchError where
  a <> _ = a

instance Printer PatternMatchError where
  prettyDoc = vsep . renderPatternMatchError

instance ShowErrorComponent PatternMatchError where
  showErrorComponent = T.unpack . prettyPrint
  errorComponentLen pmErr = let (_, len) = getErrorPos pmErr in len

type Start = Int

type Length = Int

fromAnnotation :: Annotation -> (Start, Length)
fromAnnotation (Location a b) = (a, b - a)
fromAnnotation _ = (0, 0)

getErrorPos :: PatternMatchError -> (Start, Length)
getErrorPos (EmptyPatternMatch ann) = fromAnnotation ann
getErrorPos (ConstructorArgumentLengthMismatch ann _ _ _) = fromAnnotation ann
getErrorPos (MissingPatterns ann _) = fromAnnotation ann
getErrorPos (RedundantPatterns ann _) = fromAnnotation ann
getErrorPos (DuplicateVariableUse ann _) = fromAnnotation ann

-----

renderPatternMatchError :: PatternMatchError -> [Doc ann]
renderPatternMatchError (EmptyPatternMatch _) =
  ["Pattern match needs at least one pattern to match"]
renderPatternMatchError
  ( ConstructorArgumentLengthMismatch
      _
      tyCon
      expected
      actual
    ) =
    [ "Constructor argument length mismatch. "
        <> prettyDoc tyCon
        <> " expected "
        <> prettyDoc expected
        <> " but got "
        <> prettyDoc actual
    ]
renderPatternMatchError (MissingPatterns _ missing) =
  ["Pattern match is not exhaustive. These patterns are missing:"]
    <> (prettyDoc <$> missing)
renderPatternMatchError (RedundantPatterns _ redundant) =
  ["Pattern match has unreachable patterns, you should remove them"] <> (prettyDoc <$> redundant)
renderPatternMatchError (DuplicateVariableUse _ vars) =
  [ "Pattern match variables must be unique.",
    "Variables " <> prettyDoc vars <> " are used multiple times"
  ]
