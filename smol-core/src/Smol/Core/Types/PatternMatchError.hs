{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Smol.Core.Types.PatternMatchError
  ( PatternMatchError (..),
    renderPatternMatchError,
  )
where

import Data.Set (Set)
import qualified Data.Text as T
import qualified Prettyprinter as PP
import Smol.Core.Types
import Text.Megaparsec

data PatternMatchError ann
  = -- | No patterns provided
    EmptyPatternMatch ann
  | -- | "Just 1 2" or "Nothing 3", for instance
    -- | ann, offending tyCon, expected, actual
    ConstructorArgumentLengthMismatch ann Constructor Int Int
  | -- | Cases not covered in pattern matches
    -- | ann, [missing patterns]
    MissingPatterns ann [Pattern ResolvedDep ann]
  | -- | Unnecessary cases covered by previous matches
    RedundantPatterns ann [Pattern ResolvedDep ann]
  | -- | Multiple instances of the same variable
    DuplicateVariableUse ann (Set (ResolvedDep Identifier))
  deriving stock (Eq, Ord, Show, Foldable)

------

instance Semigroup (PatternMatchError ann) where
  a <> _ = a

instance PP.Pretty (PatternMatchError ann) where
  pretty = vsep . renderPatternMatchError

instance ShowErrorComponent (PatternMatchError Annotation) where
  showErrorComponent = T.unpack . renderWithWidth 40 . PP.pretty
  errorComponentLen pmErr = let (_, len) = getErrorPos pmErr in len

type Start = Int

type Length = Int

-- | Single combined error area for Megaparsec
fromAnnotation :: Annotation -> (Start, Length)
fromAnnotation (Location a b) = (a, b - a)

getErrorPos :: PatternMatchError Annotation -> (Start, Length)
getErrorPos = fromAnnotation . mconcat . getAllAnnotations

getAllAnnotations :: PatternMatchError ann -> [ann]
getAllAnnotations = foldMap pure

-----

renderPatternMatchError ::
  PatternMatchError ann ->
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
