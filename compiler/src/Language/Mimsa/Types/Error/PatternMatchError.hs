{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types.Error.PatternMatchError
  ( PatternMatchErrorF (..),
    PatternMatchError,
    renderPatternMatchError,
  )
where

import Data.Set (Set)
import qualified Data.Text as T
import Language.Mimsa.Printer
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Prettyprinter
import Text.Megaparsec

data PatternMatchErrorF ann
  = -- | No patterns provided
    EmptyPatternMatch ann
  | -- | "Just 1 2" or "Nothing 3", for instance
    -- | ann, offending tyCon, expected, actual
    ConstructorArgumentLengthMismatch ann TyCon Int Int
  | -- | Cases not covered in pattern matches
    -- | ann, [missing patterns]
    MissingPatterns ann [Pattern Name ann]
  | -- | Unnecessary cases covered by previous matches
    RedundantPatterns ann [Pattern Name ann]
  | -- | Multiple instances of the same variable
    DuplicateVariableUse ann (Set Name)
  deriving stock (Eq, Ord, Show, Foldable)

type PatternMatchError = PatternMatchErrorF Annotation

------

instance Semigroup (PatternMatchErrorF ann) where
  a <> _ = a

instance (Printer ann) => Printer (PatternMatchErrorF ann) where
  prettyDoc = vsep . renderPatternMatchError

instance ShowErrorComponent PatternMatchError where
  showErrorComponent = T.unpack . prettyPrint
  errorComponentLen pmErr = let (_, len) = getErrorPos pmErr in len

type Start = Int

type Length = Int

-- | Single combined error area for Megaparsec
fromAnnotation :: Annotation -> (Start, Length)
fromAnnotation (Location a b) = (a, b - a)
fromAnnotation _ = (0, 0)

getErrorPos :: PatternMatchError -> (Start, Length)
getErrorPos = fromAnnotation . mconcat . getAllAnnotations

getAllAnnotations :: PatternMatchError -> [Annotation]
getAllAnnotations = foldMap pure

-----

renderPatternMatchError ::
  PatternMatchErrorF ann ->
  [Doc a]
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
