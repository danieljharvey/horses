{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Backend.Javascript where

import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers

newtype Javascript = Javascript Text

instance IsString Javascript where
  fromString = Javascript . T.pack

outputLiteral :: Literal -> Javascript
outputLiteral MyUnit = "{}"
outputLiteral (MyString s) = _

outputJS :: Expr Name -> Javascript
outputJS expr =
  case expr of
    MyLiteral a -> outputLiteral a
