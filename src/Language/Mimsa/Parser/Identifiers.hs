{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Parser.Identifiers
  ( varParser,
    nameParser,
    tyConParser,
    constructorParser,
  )
where

import Control.Monad ((>=>))
import Data.Char as Char
import Data.Text (Text)
import Language.Mimsa.Parser.Helpers
import Language.Mimsa.Parser.Types
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Text.Megaparsec

----

varParser :: Parser ParserExpr
varParser = withLocation MyVar nameParser

---

identifier :: Parser Text
identifier = takeWhile1P (Just "variable name") Char.isAlphaNum

nameParser :: Parser Name
nameParser =
  maybePred
    identifier
    (inProtected >=> safeMkName)

---

constructorParser :: Parser ParserExpr
constructorParser = withLocation MyConstructor tyConParser

tyConParser :: Parser TyCon
tyConParser =
  maybePred
    identifier
    (inProtected >=> safeMkTyCon)
-----
