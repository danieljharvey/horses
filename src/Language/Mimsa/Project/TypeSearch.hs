module Language.Mimsa.Project.TypeSearch
  ( NormalisedMonoType (..),
    typeSearch,
    typeSearchFromText,
  )
where

import Data.Bifunctor (first)
import Data.Functor
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import Language.Mimsa.Parser.Helpers (parseAndFormat)
import Language.Mimsa.Parser.MonoType
import Language.Mimsa.Typechecker.NormaliseTypes
import Language.Mimsa.Types.AST.Annotation
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Typechecker

newtype NormalisedMonoType
  = Normalised MonoType

instance Eq NormalisedMonoType where
  (Normalised mtA) == (Normalised mtB) =
    normalise mtA == normalise mtB
    where
      normalise a =
        normaliseType a $> ()

typeSearch :: Map Name MonoType -> MonoType -> Map Name MonoType
typeSearch items mt = M.filter (\mt' -> Normalised mt' == Normalised mt) items

typeSearchFromText ::
  Map Name MonoType ->
  Text ->
  Either (Error Annotation) (Map Name MonoType)
typeSearchFromText items input = do
  mt <- first OtherError (parseAndFormat monoTypeParser input)
  pure $ typeSearch items mt
