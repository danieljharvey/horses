{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types.Variable
  ( Variable (..),
    BiIds (..),
  )
where

import qualified Data.Text as T
import Data.Text.Prettyprint.Doc
import GHC.Generics
import Language.Mimsa.Printer
import Language.Mimsa.Types.Name

data BiIds
  = NoId
  | OneId Variable
  | TwoIds Variable Variable
  | ThreeIds Variable Variable Variable
  deriving (Eq, Ord, Show, Generic)

data Variable
  = NamedVar Name
  | NumberedVar Int
  | BuiltIn Name
  | BuiltInActual Name BiIds
  deriving (Eq, Ord, Show, Generic)

instance Printer BiIds where
  prettyPrint NoId = "-"
  prettyPrint (OneId v1) = prettyPrint v1
  prettyPrint (TwoIds v1 v2) = T.intercalate ", " (prettyPrint <$> [v1, v2])
  prettyPrint (ThreeIds v1 v2 v3) = T.intercalate ", " (prettyPrint <$> [v1, v2, v3])

instance Printer Variable where
  prettyDoc = renderVariable

renderVariable :: Variable -> Doc ann
renderVariable (NamedVar n) = renderName n
renderVariable (NumberedVar i) = "U" <> pretty i
renderVariable (BuiltIn n) = renderName n
renderVariable (BuiltInActual n _) = renderName n
