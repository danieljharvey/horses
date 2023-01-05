module Language.Mimsa.Core.Parser.Lexeme where

import Control.Applicative
import Data.Functor (($>))
import Data.Text (Text)
import Language.Mimsa.Core.Parser.Types
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

myLexeme :: Parser a -> Parser a
myLexeme = L.lexeme (L.space space1 empty empty)

myString :: Text -> Parser ()
myString s = myLexeme (string s) $> ()
