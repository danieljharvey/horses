{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

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
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import qualified Language.Mimsa.Types.Error.TypeError as TE
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
  (MonoType, Set FoundPath) ->
  Maybe TypedHoleResponse
mkTypedHoleResponse input name (mt, suggestions) =
  TypedHoleResponse (coerce name) (prettyPrint mt)
    <$> sourceSpan input (getAnnotationForType mt)
      <*> pure (prettyPrint <$> S.toList suggestions)

newtype ErrorLocation = ErrorLocation
  {elSourceSpan :: SourceSpan}
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (JSON.ToJSON, ToSchema)

mkErrorLocation ::
  Text -> Annotation -> Maybe ErrorLocation
mkErrorLocation input ann =
  ErrorLocation <$> sourceSpan input ann

data UserErrorResponse = UserErrorResponse
  { ueText :: Text,
    ueTypedHoles :: [TypedHoleResponse],
    ueErrorLocations :: [ErrorLocation]
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (JSON.ToJSON, ToSchema)

toUserError :: (Show ann, Printer ann) => Error ann -> UserErrorResponse
toUserError te@(TypeErr input typeErr) =
  let errorLocations =
        catMaybes $
          mkErrorLocation input <$> TE.getAllAnnotations typeErr
   in case typeErr of
        TypedHoles holes ->
          UserErrorResponse
            { ueText = "Typed holes found",
              ueTypedHoles =
                catMaybes
                  ( uncurry (mkTypedHoleResponse input)
                      <$> M.toList holes
                  ),
              ueErrorLocations = []
            }
        _ ->
          UserErrorResponse
            { ueText = prettyPrint te,
              ueTypedHoles = [],
              ueErrorLocations = errorLocations
            }
toUserError other =
  UserErrorResponse
    { ueText = prettyPrint other,
      ueTypedHoles = [],
      ueErrorLocations = []
    }
