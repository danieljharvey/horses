
module Smol.Core.Parser.DataType (dataTypeParser) where

import Smol.Core.Types.DataType
import Control.Monad ((>=>))
import Control.Monad.Combinators.Expr
  ( Operator (InfixR),
    makeExprParser,
  )
import Data.Bifunctor (first)
import qualified Data.Char as Char
import Data.Foldable (foldl')
import Data.Functor (($>))
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Smol.Core.Parser.Identifiers
  ( constructorParserInternal,
    identifierParser,
  )
import qualified Smol.Core.Parser.Primitives as Prim
import Smol.Core.Parser.Shared
  ( chainl1,
    commaSep,
    inBrackets,
    maybePred,
    myLexeme,
    myString,
    orInBrackets,
    withLocation,
  )
import Smol.Core.Types.Annotation (Annotation)
import Smol.Core.Types.Identifier
import Smol.Core.Types.Type
  ( Type
      ( TApp,
        TConstructor,
        TFunc,
        TLiteral,
        TPrim,
        TRecord,
        TTuple,
        TUnion,
        TVar
      ),
    TypePrim (TPBool, TPInt, TPNat),
  )
import Smol.Core.Types.TypeName (TypeName (..))
import Text.Megaparsec
  ( MonadParsec (eof, label, takeWhile1P, try),
    ParseErrorBundle,
    Parsec,
    errorBundlePretty,
    parse,
    sepBy,
    some,
    (<|>),
  )
import Text.Megaparsec.Char (space)

type Parser = Parsec Void Text

type ParseErrorType = ParseErrorBundle Text Void

dataTypeParser :: Parser (DataType Annotation)
dataTypeParser = undefined

