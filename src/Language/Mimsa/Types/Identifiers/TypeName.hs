{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types.Identifiers.TypeName where

import qualified Data.Aeson as JSON
import Data.Swagger
import Data.Text.Prettyprint.Doc
import GHC.Generics
import Language.Mimsa.Printer
import Language.Mimsa.Types.Identifiers.Name
import Language.Mimsa.Types.Identifiers.TyCon

data TypeName
  = ConsName TyCon [TypeName]
  | VarName Name
  | TNFunc TypeName TypeName
  deriving
    ( Eq,
      Ord,
      Show,
      Generic,
      JSON.ToJSON,
      JSON.FromJSON,
      ToSchema
    )

instance Printer TypeName where
  prettyDoc = renderTypeName

renderTypeName :: TypeName -> Doc ann
renderTypeName (ConsName c []) = prettyDoc c
renderTypeName (ConsName c tys) =
  encloseSep
    lparen
    rparen
    space
    ([prettyDoc c] <> (prettyDoc <$> tys))
renderTypeName (VarName v) = prettyDoc v
renderTypeName (TNFunc a b) =
  parens
    ( prettyDoc a <+> "->"
        <+> prettyDoc b
    )
