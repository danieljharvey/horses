{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Builder (doJobsIO, doJobsPure, getMissing, Plan (..), State (..), Job, Inputs) where

import qualified Control.Concurrent.STM as STM
import Control.Monad.Identity
import Data.Foldable (traverse_)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import qualified Ki
import System.IO.Unsafe

-- a thing we want to do
data Plan k input = Plan
  { jbDeps :: Set k,
    jbInput :: input
  }
  deriving stock (Eq, Ord, Show)

-- how we're going to do it
type Job m e k input output =
  Map k output -> input -> m (Either e output)

type Inputs k input =
  Map k (Plan k input)

-- state of the job
data State k input output = State
  { stInputs :: Inputs k input,
    stOutputs :: Map k output
  }
  deriving stock (Eq, Ord, Show)

-- list the required deps that cannot possibly be provided (usually indicates
-- an error with implementation)
getMissing :: (Ord k) => State k input output -> Set k
getMissing (State inputs outputs) =
  let getMissingDeps (Plan deps _) =
        S.filter
          (\dep -> dep `M.notMember` inputs && dep `M.notMember` outputs)
          deps
   in mconcat (getMissingDeps <$> M.elems inputs)

getReadyJobs :: (Ord k) => Either e (State k input output) -> Set k -> Inputs k input
getReadyJobs (Left _) _ = mempty
getReadyJobs (Right st) inFlight =
  -- disregard any jobs that are inflight
  let inputs = M.filterWithKey (\k _ -> S.notMember k inFlight) (stInputs st)
   in -- get jobs we are ready to do

      M.filter
        ( \plan ->
            let requiredKeys = jbDeps plan
             in and ((\depK -> M.member depK (stOutputs st)) <$> S.toList requiredKeys)
        )
        inputs

-- | remove job from input, add it to output
markJobAsDone ::
  (Ord k) =>
  k ->
  output ->
  Either e (State k input output) ->
  Either e (State k input output)
markJobAsDone _ _ (Left e) = Left e
markJobAsDone k output (Right st) =
  Right $
    State (M.delete k (stInputs st)) (stOutputs st <> M.singleton k output)

-- unsafely do the things
doJobsPure ::
  forall e k input output.
  (Ord k, Show k) =>
  Job Identity e k input output ->
  State k input output ->
  Either e (State k input output)
doJobsPure fn st =
  unsafePerformIO (doJobsIO ioFn st)
  where
    ioFn dep input =
      pure $ runIdentity $ fn dep input

-- run through a list of jobs and do them
doJobsIO ::
  forall e k input output.
  (Ord k, Show k) =>
  Job IO e k input output ->
  State k input output ->
  IO (Either e (State k input output))
doJobsIO fn st = do
  let missingDeps = getMissing st
  if not (S.null missingDeps)
    then error ("Missing deps in build: " <> show missingDeps)
    else do
      Ki.scoped $ \scope -> do
        mutableState <- STM.newTVarIO (Right st)
        inFlight <- STM.newTVarIO mempty -- list of keys currently being built
        let getReadyJobsIO =
              getReadyJobs <$> STM.readTVarIO mutableState <*> STM.readTVarIO inFlight

        readyJobs <- getReadyJobsIO

        let doJob :: (k, Plan k input) -> IO ()
            doJob (k, plan) = do
              -- mark this job as inflight
              filteredOutput <- STM.atomically $ do
                STM.modifyTVar' inFlight (S.insert k)
                eitherState <- STM.readTVar mutableState
                pure $ case eitherState of
                  Left _ -> mempty
                  Right state ->
                    M.filterWithKey
                      (\depK _ -> S.member depK (jbDeps plan))
                      (stOutputs state)

              -- do the work
              newOutput <- fn filteredOutput (jbInput plan)

              case newOutput of
                Left e -> STM.atomically $ STM.writeTVar mutableState (Left e)
                Right success -> do
                  -- update the state
                  _ <- STM.atomically $ do
                    STM.modifyTVar' mutableState (markJobAsDone k success)
                    STM.modifyTVar' inFlight (S.delete k)

                  -- get the resulting jobs
                  newReadyJobs <- getReadyJobsIO

                  -- run them
                  traverse_ (Ki.fork scope . doJob) (M.toList newReadyJobs)

        -- start first jobs
        traverse_ (Ki.fork scope . doJob) (M.toList readyJobs)

        -- wait for all the sillyness to stop
        STM.atomically $ Ki.awaitAll scope

        -- read the var and give up
        STM.readTVarIO mutableState

-- get jobs available to start, fork them, and add key to `inFlight`
-- each one, when done, updates state, and then checks again what can
-- be started, and forks those
-- when no more inputs (and nothing else in flight, return state)
