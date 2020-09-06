{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Language.Mimsa.Types.TypeName where

import qualified Data.Aeson as JSON
import Data.Text.Prettyprint.Doc
import GHC.Generics
import Language.Mimsa.Printer
import Language.Mimsa.Types.Construct
import Language.Mimsa.Types.Name

data TypeName = ConsName Construct [TypeName] | VarName Name
  deriving (Eq, Ord, Show, Generic, JSON.ToJSON, JSON.FromJSON)

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
