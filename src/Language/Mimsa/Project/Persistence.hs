{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Project.Persistence
  ( loadProject,
    saveProject,
    getCurrentBindings,
    getCurrentTypeBindings,
  )
where

-- functions for Projects as opposed to the larger Store
import Control.Exception
import Control.Monad.Except
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy as BS
import Data.Coerce
import Data.Functor
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Language.Mimsa.Store.Storage
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.Store

type PersistApp a = ExceptT Text IO a

hush :: Either IOError a -> Maybe a
hush (Right a) = pure a
hush _ = Nothing

-- load environment.json and any hashed exprs mentioned in it
-- should probably consider loading the exprs lazily as required in future
loadProject :: (Monoid ann) => PersistApp (Project ann)
loadProject = do
  proj <- loadProject'
  pure $ proj $> mempty

loadProject' ::
  PersistApp (Project ())
loadProject' = do
  project' <- liftIO $ try $ BS.readFile envPath
  case hush project' >>= JSON.decode of
    Just sp -> do
      store' <-
        recursiveLoadBoundExpressions
          (projectServers sp)
          (getItemsForAllVersions . projectBindings $ sp)
      typeStore' <-
        recursiveLoadBoundExpressions
          (projectServers sp)
          (getItemsForAllVersions . projectTypes $ sp)
      pure $ projectFromSaved (store' <> typeStore') sp
    _ -> throwError $ "Could not decode file at " <> T.pack envPath

saveProject :: Project ann -> PersistApp ()
saveProject p = saveProject' (p $> ())

saveProject' :: Project () -> PersistApp ()
saveProject' env = do
  let jsonStr = JSON.encode (projectToSaved env)
  liftIO $ BS.writeFile envPath jsonStr

--

loadBoundExpressions ::
  [ServerUrl] ->
  Set ExprHash ->
  PersistApp (Store ())
loadBoundExpressions urls hashes = do
  items' <-
    traverse
      ( \hash -> do
          item <- findExpr urls hash
          pure (hash, item)
      )
      (S.toList hashes)
  pure
    (Store (M.fromList items'))

recursiveLoadBoundExpressions ::
  [ServerUrl] ->
  Set ExprHash ->
  PersistApp (Store ())
recursiveLoadBoundExpressions urls hashes = do
  store' <- loadBoundExpressions urls hashes
  let newHashes =
        S.difference
          ( S.unions $
              getDependencyHashes <$> M.elems (getStore store')
          )
          hashes
  if S.null newHashes
    then pure store'
    else do
      moreStore <- recursiveLoadBoundExpressions urls newHashes
      pure (store' <> moreStore)

--
storePath :: String
storePath = "./"

envPath :: String
envPath = storePath <> "environment.json"

getCurrentBindings :: VersionedBindings -> Bindings
getCurrentBindings versioned =
  Bindings (NE.last <$> getVersionedMap versioned)

getCurrentTypeBindings :: VersionedTypeBindings -> TypeBindings
getCurrentTypeBindings versioned =
  TypeBindings (NE.last <$> getVersionedMap versioned)

getItemsForAllVersions :: (Ord a) => VersionedMap k a -> Set a
getItemsForAllVersions versioned =
  mconcat $ M.elems (S.fromList . NE.toList <$> coerce versioned)

getDependencyHashes :: StoreExpression ann -> Set ExprHash
getDependencyHashes =
  S.fromList . M.elems . getBindings . storeBindings
