{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Language.Mimsa.Server.Project
  ( projectEndpoints,
    ProjectAPI,
  )
where

import Control.Monad.Except
import qualified Data.Aeson as JSON
import Data.Functor
import Data.Text (Text)
import qualified Data.Text.IO as T
import Data.Text.Lazy (fromStrict)
import Data.Text.Lazy.Encoding (encodeUtf8)
import GHC.Generics
import Language.Mimsa.Actions (evaluateText)
import Language.Mimsa.Printer
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.ResolvedExpression
import Servant

newtype EvaluateBody
  = EvaluateBody {code :: Text}
  deriving (Eq, Ord, Show, Generic, JSON.FromJSON)

type ProjectAPI =
  "project" :> ("evaluate" :> ReqBody '[JSON] EvaluateBody :> Post '[JSON] (ResolvedExpression ()))

projectEndpoints :: Project Annotation -> Server ProjectAPI
projectEndpoints = evaluateExpression

evaluateExpression ::
  Project Annotation ->
  EvaluateBody ->
  Handler (ResolvedExpression ())
evaluateExpression project body = do
  liftIO $ T.putStrLn (code body)
  case evaluateText project (code body) of
    Left e -> throwError (to400Error e)
    Right a -> pure (a $> ())

to400Error :: (Printer a) => a -> ServerError
to400Error a = err400 {errBody = buildMsg a}
  where
    buildMsg = encodeUtf8 . fromStrict . prettyPrint
