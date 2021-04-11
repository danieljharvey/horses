{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Router.Environment
  ( ExprHash (..),
    Log (..),
    Key (..),
    Environment (..),
    newEnvironment,
    addRoute,
    lookupRoute,
  )
where

import qualified Control.Concurrent.STM as STM
import Control.Monad.IO.Class
import qualified Data.Aeson as JSON
import Data.Map (Map)
import qualified Data.Map as M
import Data.Semigroup
import Data.Text (Text)
import GHC.Generics

-- | newtype for ExprHash
newtype ExprHash = ExprHash Text
  deriving (Eq, Ord, Show, Generic, JSON.FromJSON, JSON.ToJSON)

-- everything we'll log to find out if a given route is working
data Log = Log
  { logExprHash :: ExprHash,
    logRoute :: Text,
    logDuration :: Int,
    logStatusCode :: Int
  }

-- the nice name for our routes
-- this will allow us to switch things behind the scenes if we wish to be
-- clever in future
newtype Key = Key Int
  deriving (Eq, Ord, Show, Bounded, Num, JSON.ToJSON, Read)

-- our state - all of the routes we've set up
data Environment = Environment
  { envRoutes :: STM.TVar (Map Key ExprHash),
    envStats :: STM.TVar (Map Key Log)
  }

-- | Create a new environment with no routes or stats
newEnvironment :: (MonadIO m) => m Environment
newEnvironment =
  Environment <$> liftIO (STM.newTVarIO mempty)
    <*> liftIO (STM.newTVarIO mempty)

getNextKey :: Map Key ExprHash -> Key
getNextKey = max 1 . (+ 1) . getMax . foldMap Max . M.keys

-- adds a new route
addRoute :: (MonadIO m) => Environment -> ExprHash -> m Key
addRoute env exprHash = do
  let updateTVar = \routes -> do
        let nextKey = getNextKey routes
        (nextKey, routes <> M.singleton nextKey exprHash)
  liftIO $
    STM.atomically $
      STM.stateTVar
        (envRoutes env)
        updateTVar

-- looks up the ExprHash for a given key
lookupRoute :: (MonadIO m) => Environment -> Key -> m (Maybe ExprHash)
lookupRoute env key =
  M.lookup key <$> liftIO (STM.readTVarIO (envRoutes env))
