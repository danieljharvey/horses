{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Store
  ( loadEnvironment,
    loadBoundExpressions,
    saveEnvironment,
    getCurrentBindings,
    module Language.Mimsa.Store.Storage,
    module Language.Mimsa.Store.Resolver,
    module Language.Mimsa.Store.Substitutor,
    module Language.Mimsa.Store.ResolvedDeps,
    module Language.Mimsa.Store.DepGraph,
  )
where

import Control.Exception (try)
import Control.Monad.Trans.Except
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy as BS
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import Language.Mimsa.Store.DepGraph
import Language.Mimsa.Store.ResolvedDeps
import Language.Mimsa.Store.Resolver
import Language.Mimsa.Store.Storage
import Language.Mimsa.Store.Substitutor
import Language.Mimsa.Types
  ( Bindings (..),
    ExprHash (..),
    Store (..),
    Project (..),
    StoreExpression (..),
    VersionedBindings (..),
  )

storePath :: String
storePath = "./"

envPath :: String
envPath = storePath <> "environment.json"

hush :: Either IOError a -> Maybe a
hush (Right a) = Just a
hush _ = Nothing

getCurrentBindings :: VersionedBindings -> Bindings
getCurrentBindings (VersionedBindings versioned) = Bindings (NE.last <$> versioned)

getHashesForAllVersions :: VersionedBindings -> Set ExprHash
getHashesForAllVersions (VersionedBindings versioned) =
  mconcat $ M.elems (S.fromList . NE.toList <$> versioned)

getDependencyHashes :: StoreExpression -> Set ExprHash
getDependencyHashes = S.fromList . M.elems . getBindings . storeBindings

-- load environment.json and any hashed exprs mentioned in it
-- should probably consider loading the exprs lazily as required in future
loadEnvironment :: IO (Maybe Project)
loadEnvironment = do
  envJson <- try $ BS.readFile envPath
  case hush envJson >>= JSON.decode of
    Just vb@(VersionedBindings _) -> do
      items' <- runExceptT $ recursiveLoadBoundExpressions (getHashesForAllVersions vb)
      case items' of
        Right store' -> pure $ Just (Project store' vb)
        _ -> pure Nothing
    _ -> pure Nothing

--

loadBoundExpressions :: Set ExprHash -> ExceptT Text IO Store
loadBoundExpressions hashes = do
  items' <-
    traverse
      ( \hash -> do
          item <- findExpr hash
          pure (hash, item)
      )
      (S.toList hashes)
  pure
    (Store (M.fromList items'))

recursiveLoadBoundExpressions :: Set ExprHash -> ExceptT Text IO Store
recursiveLoadBoundExpressions hashes = do
  store' <- loadBoundExpressions hashes
  let newHashes =
        S.difference
          ( S.unions $
              getDependencyHashes <$> (M.elems (getStore store'))
          )
          hashes
  if S.null newHashes
    then pure store'
    else do
      moreStore <- recursiveLoadBoundExpressions newHashes
      pure (store' <> moreStore)

--
saveEnvironment :: Project -> IO ()
saveEnvironment env = do
  let jsonStr = JSON.encode (bindings env)
  BS.writeFile envPath jsonStr
