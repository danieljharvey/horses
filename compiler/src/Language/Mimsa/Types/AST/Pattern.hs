{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types.AST.Pattern
  ( Pattern (..),
    printSubPattern,
    getPatternAnnotation,
  )
where

import qualified Data.Aeson as JSON
import Data.Map (Map)
import qualified Data.Map as M
import GHC.Generics
import Language.Mimsa.Printer
import Language.Mimsa.Types.AST.Literal
import Language.Mimsa.Types.AST.Spread
import Language.Mimsa.Types.AST.StringPart
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Utils
import Prettyprinter

data Pattern var ann
  = PWildcard
      { patAnn :: ann
      }
  | PVar
      { patAnn :: ann,
        patVar :: var
      }
  | PLit
      { patAnn :: ann,
        patLiteral :: Literal
      }
  | PConstructor
      { patAnn :: ann,
        patTyCon :: TyCon,
        patMatches :: [Pattern var ann]
      }
  | PPair
      { patAnn :: ann,
        patA :: Pattern var ann,
        patB :: Pattern var ann
      }
  | PRecord
      { patAnn :: ann,
        patMap ::
          Map Name (Pattern var ann)
      }
  | PArray
      { patAnn :: ann,
        patArray :: [Pattern var ann],
        patSpread :: Spread var ann
      }
  | PString
      { patAnn :: ann,
        patHead :: StringPart var ann,
        patTail :: StringPart var ann
      }
  deriving stock (Show, Eq, Ord, Functor, Foldable, Generic)
  deriving anyclass (JSON.FromJSON, JSON.ToJSON)

getPatternAnnotation :: Pattern var ann -> ann
getPatternAnnotation (PWildcard ann) = ann
getPatternAnnotation (PVar ann _) = ann
getPatternAnnotation (PLit ann _) = ann
getPatternAnnotation (PConstructor ann _ _) = ann
getPatternAnnotation (PPair ann _ _) = ann
getPatternAnnotation (PRecord ann _) = ann
getPatternAnnotation (PArray ann _ _) = ann
getPatternAnnotation (PString ann _ _) = ann

inParens :: (Printer a) => a -> Doc style
inParens = parens . prettyDoc

-- print simple things with no brackets, and complex things inside brackets
printSubPattern :: Pattern Name ann -> Doc style
printSubPattern pat = case pat of
  all'@PConstructor {} -> inParens all'
  a -> prettyDoc a

instance Printer (Pattern Name ann) where
  prettyDoc (PWildcard _) = "_"
  prettyDoc (PVar _ a) = prettyDoc a
  prettyDoc (PLit _ lit) = prettyDoc lit
  prettyDoc (PConstructor _ tyCon []) =
    prettyDoc tyCon
  prettyDoc (PConstructor _ tyCon args) =
    prettyDoc tyCon <> foldr (\a b -> " " <> a <> b) mempty (printSubPattern <$> args)
  prettyDoc (PPair _ a b) =
    "(" <> prettyDoc a <> ", " <> prettyDoc b <> ")"
  prettyDoc (PArray _ as spread) =
    "[" <> concatWith (\a b -> a <> ", " <> b) (prettyDoc <$> as) <> prettyDoc spread <> "]"
  prettyDoc (PRecord _ map') =
    let items = M.toList map'
        printRow i (name, val) =
          let item = case val of
                (PVar _ vName) | vName == name -> prettyDoc name
                _ ->
                  prettyDoc name
                    <> ":"
                    <+> printSubPattern val
           in item <> if i < length items then "," else ""
     in case items of
          [] -> "{}"
          rows ->
            let prettyRows = mapWithIndex printRow rows
             in group
                  ( "{"
                      <+> align
                        ( vsep
                            prettyRows
                        )
                      <+> "}"
                  )
  prettyDoc (PString _ a as) =
    prettyDoc a <> " ++ " <> prettyDoc as
