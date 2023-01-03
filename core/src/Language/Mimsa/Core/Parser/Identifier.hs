module Language.Mimsa.Core.Parser.Identifier
  ( identifierParser,
  )
where

import Language.Mimsa.Core.Parser.Helpers
import Language.Mimsa.Core.Parser.Identifiers
import Language.Mimsa.Core.Parser.Lexeme
import Language.Mimsa.Core.Parser.Types
import Language.Mimsa.Core.Types.AST
import Language.Mimsa.Core.Types.Identifiers

----

identifierParser :: Parser (Identifier Name Annotation)
identifierParser =
  plainIdentifierParser

plainIdentifierParser :: Parser (Identifier Name Annotation)
plainIdentifierParser =
  myLexeme
    ( withLocation
        Identifier
        nameParserInt
    )
