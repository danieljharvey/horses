{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Language.Mimsa.Server.Errors.UserErrorResponse
  ( UserErrorResponse (..),
    toUserError,
  )
where

import qualified Data.Aeson as JSON
import Data.Coerce (coerce)
import qualified Data.Map as M
import Data.Maybe (catMaybes)
import Data.OpenApi hiding (name)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import GHC.Generics
import Language.Mimsa.Printer
import Language.Mimsa.Project.SourceSpan
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Project.SourceSpan
import Language.Mimsa.Types.Typechecker

data TypedHoleResponse = TypedHoleResponse
  { thName :: Text,
    thMonoType :: Text,
    thSourceSpan :: SourceSpan,
    thSuggestions :: [Text]
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (JSON.ToJSON, ToSchema)

mkTypedHoleResponse ::
  Text ->
  Name ->
  (MonoType, Set Name) ->
  Maybe TypedHoleResponse
mkTypedHoleResponse input name (mt, suggestions) =
  TypedHoleResponse (coerce name) (prettyPrint mt)
    <$> sourceSpan input (getAnnotationForType mt)
      <*> pure (coerce <$> S.toList suggestions)

data UserErrorResponse = UserErrorResponse
  { ueText :: Text,
    ueTypedHoles :: [TypedHoleResponse]
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (JSON.ToJSON, ToSchema)

toUserError :: (Show ann, Printer ann) => Error ann -> UserErrorResponse
toUserError te@(TypeErr input typeErr) =
  case typeErr of
    TypedHoles holes ->
      UserErrorResponse
        { ueText = prettyPrint typeErr,
          ueTypedHoles =
            catMaybes
              ( uncurry (mkTypedHoleResponse input)
                  <$> M.toList holes
              )
        }
    _ ->
      UserErrorResponse
        { ueText = prettyPrint te,
          ueTypedHoles = []
        }
toUserError other =
  UserErrorResponse
    { ueText = prettyPrint other,
      ueTypedHoles = []
    }
