{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types.AST.Field where

import qualified Data.Aeson as JSON
import Data.Swagger
import Data.Text.Prettyprint.Doc
import GHC.Generics
import Language.Mimsa.Printer
import Language.Mimsa.Types.Identifiers.Name
import Language.Mimsa.Types.Identifiers.TyCon

data Field
  = ConsName TyCon [Field]
  | VarName Name
  | TNFunc Field Field
  deriving
    ( Eq,
      Ord,
      Show,
      Generic,
      JSON.ToJSON,
      JSON.FromJSON,
      ToSchema
    )

instance Printer Field where
  prettyDoc = renderField

renderField :: Field -> Doc ann
renderField (ConsName c []) = prettyDoc c
renderField (ConsName c tys) =
  encloseSep
    lparen
    rparen
    space
    ([prettyDoc c] <> (prettyDoc <$> tys))
renderField (VarName v) = prettyDoc v
renderField (TNFunc a b) =
  parens
    ( prettyDoc a <+> "->"
        <+> prettyDoc b
    )
