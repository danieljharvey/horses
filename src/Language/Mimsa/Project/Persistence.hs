{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Project.Persistence
  ( loadProject,
    saveProject,
    getCurrentBindings,
  )
where

-- functions for Projects as opposed to the larger Store

import Control.Exception (try)
import Control.Monad.Trans.Except
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy as BS
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import Language.Mimsa.Store.Storage
import Language.Mimsa.Types
  ( Bindings (..),
    ExprHash (..),
    Project (..),
    ServerUrl (..),
    Store (..),
    StoreExpression (..),
    VersionedBindings (..),
  )

servers :: [ServerUrl]
servers = pure (ServerUrl "https://raw.githubusercontent.com/danieljharvey/mimsa-store/master/")

-- load environment.json and any hashed exprs mentioned in it
-- should probably consider loading the exprs lazily as required in future
loadProject :: IO (Maybe Project)
loadProject = do
  envJson <- try $ BS.readFile envPath
  case hush envJson >>= JSON.decode of
    Just vb@(VersionedBindings _) -> do
      items' <- runExceptT $ recursiveLoadBoundExpressions servers (getHashesForAllVersions vb)
      case items' of
        Right store' -> pure $ Just (Project store' vb servers)
        _ -> pure Nothing
    _ -> pure Nothing

saveProject :: Project -> IO ()
saveProject env = do
  let jsonStr = JSON.encode (bindings env)
  BS.writeFile envPath jsonStr

--

loadBoundExpressions :: [ServerUrl] -> Set ExprHash -> ExceptT Text IO Store
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

recursiveLoadBoundExpressions :: [ServerUrl] -> Set ExprHash -> ExceptT Text IO Store
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

hush :: Either IOError a -> Maybe a
hush (Right a) = Just a
hush _ = Nothing

getCurrentBindings :: VersionedBindings -> Bindings
getCurrentBindings (VersionedBindings versioned) =
  Bindings (NE.last <$> versioned)

getHashesForAllVersions :: VersionedBindings -> Set ExprHash
getHashesForAllVersions (VersionedBindings versioned) =
  mconcat $ M.elems (S.fromList . NE.toList <$> versioned)

getDependencyHashes :: StoreExpression -> Set ExprHash
getDependencyHashes =
  S.fromList . M.elems . getBindings . storeBindings
