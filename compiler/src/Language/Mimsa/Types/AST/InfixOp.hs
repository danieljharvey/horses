{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types.AST.InfixOp where

import qualified Data.Aeson as JSON
import Data.Swagger
import Data.Text (Text)
import qualified Data.Text as T
import Language.Mimsa.Printer

------------

newtype InfixOp = InfixOp Text
  deriving newtype
    ( Eq,
      Ord,
      Show,
      JSON.ToJSON,
      JSON.FromJSON,
      ToSchema
    )

-------------

validChars :: String
validChars = "<!@£$%^&*~|-_+=>"

validInfixOp :: Text -> Bool
validInfixOp a =
  T.length a > 1
    && T.filter (`elem` validChars) a == a

mkInfixOp :: Text -> InfixOp
mkInfixOp a =
  if validInfixOp a
    then InfixOp a
    else error $ T.unpack $ "InfixOp validation fail for '" <> a <> "'"

safeMkInfixOp :: Text -> Maybe InfixOp
safeMkInfixOp a =
  if validInfixOp a
    then Just (InfixOp a)
    else Nothing

instance Printer InfixOp where
  prettyPrint (InfixOp t) = t
