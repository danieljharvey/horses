{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
  {-# LANGUAGE RankNTypes #-}
    {-# LANGUAGE ScopedTypeVariables #-}
module Language.Mimsa.Actions.Helpers.Build (doJobs, doJobsIO, getMissing, Plan (..), State (..), Job, Inputs) where

import Basement.Monad
import Control.Monad.IO.Class
import Data.Foldable (traverse_)
import qualified Control.Concurrent.STM as STM
import Control.Parallel.Strategies
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import Language.Mimsa.Core (Printer (..))
import qualified Ki

-- a thing we want to do
data Plan k input = Plan
  { jbDeps :: Set k,
    jbInput :: input
  }
  deriving stock (Eq, Ord, Show)

instance (Printer k, Printer input) => Printer (Plan k input) where
  prettyPrint (Plan deps _input) = prettyPrint deps

-- how we're going to do it
type Job m k input output = Map k output -> input -> m output

type Inputs k input = Map k (Plan k input)

-- state of the job
data State k input output = State
  { stInputs :: Inputs k input,
    stOutputs :: Map k output
  }
  deriving stock (Eq, Ord, Show)

instance (Printer k, Printer input, Printer output) => Printer (State k input output) where
  prettyPrint (State inputs _outputs) =
    prettyPrint inputs

-- | one run of the builder builds everything that is currently ready, then
-- updates the state
runBuilder ::
  (Ord k, Monad m) =>
  Job m k input output ->
  State k input output ->
  m (State k input output)
runBuilder fn st = do
  -- filter out finished stuff from inputs (lets us start with cached results)
  let newState = filterDoneWork st

  -- get jobs we are ready to do
  let readyJobs =
        M.filter
          ( \plan ->
              let requiredKeys = jbDeps plan
               in and ((\depK -> M.member depK (stOutputs st)) <$> S.toList requiredKeys)
          )
          (stInputs newState)

  -- do the jobs
  done <-
    traverse
      ( \(k, plan) -> do
          let filteredOutput =
                M.filterWithKey
                  (\depK _ -> S.member depK (jbDeps plan))
                  (stOutputs st)
          output <- fn filteredOutput (jbInput plan)
          pure (k, output)
      )
      (M.toList readyJobs)

  -- evaluate everything in parallel
  let reallyDone = done `using` parTraversable rseq

  -- remove them from inputs
  let newInputs =
        M.filterWithKey
          (\k _ -> S.notMember k (M.keysSet readyJobs))
          (stInputs newState)

  -- add them to outputs
  pure (State newInputs (stOutputs st <> M.fromList reallyDone))

-- list the required deps that cannot possibly be provided (usually indicates
-- an error with implementation)
getMissing :: (Ord k) => State k input output -> Set k
getMissing (State inputs outputs) =
  let getMissingDeps (Plan deps _) =
        S.filter
          (\dep -> dep `M.notMember` inputs && dep `M.notMember` outputs)
          deps
   in mconcat (getMissingDeps <$> M.elems inputs)


-- run through a list of jobs and do them
doJobs ::
  (Ord k, Show k, Monad m, Eq input, Eq output) =>
  Job m k input output ->
  State k input output ->
  m (State k input output)
doJobs fn st = do
  let missingDeps = getMissing st
  if not (S.null missingDeps)
    then error ("Missing deps in build: " <> show missingDeps)
    else do
      newState <- runBuilder fn st
      if M.null (stInputs newState) || newState == st -- no more inputs, or there was no change (to stop infinite loop)
        then pure newState
        else doJobs fn newState

-- some stuff might already be completed, don't need to do it
filterDoneWork :: (Ord k) => State k input output -> State k input output
filterDoneWork st =
  st { stInputs = M.filterWithKey (\k _ -> M.notMember k (stOutputs st)) (stInputs st) }

getReadyJobs :: (Ord k) => State k input output -> Set k -> Inputs k input
getReadyJobs st inFlight =
  -- disregard any jobs that are inflight
  let inputs = M.filterWithKey (\k _ -> S.notMember k inFlight) (stInputs st)
  -- get jobs we are ready to do
   in
        M.filter
          ( \plan ->
              let requiredKeys = jbDeps plan
               in and ((\depK -> M.member depK (stOutputs st)) <$> S.toList requiredKeys)
          )
          inputs

-- | remove job from input, add it to output
markJobAsDone :: (Ord k) => k -> output -> State k input output -> State k input output
markJobAsDone k output st =
  State (M.delete k (stInputs st)) (stOutputs st <> M.singleton k output )

-- run through a list of jobs and do them
doJobsIO ::
  forall m k input output.
  (Ord k, Show k, MonadIO m, PrimMonad m) =>
  Job m k input output ->
  State k input output ->
  m (State k input output)
doJobsIO fn st = do
  let missingDeps = getMissing st
  if not (S.null missingDeps)
    then error ("Missing deps in build: " <> show missingDeps)
    else do
      liftIO $ Ki.scoped $ \scope -> do
        mutableState <- liftIO $ STM.newTVarIO st
        inFlight <- liftIO $ STM.newTVarIO mempty -- list of keys currently being built

        let getReadyJobsIO = getReadyJobs <$> STM.readTVarIO mutableState <*> STM.readTVarIO inFlight

        readyJobs <- getReadyJobsIO

        let doJob :: (k, Plan k input) -> m ()
            doJob (k, plan) =  do
              -- mark this job as inflight
              filteredOutput <- liftIO $ STM.atomically $ do
                STM.modifyTVar' inFlight (S.insert k)
                state <- STM.readTVar mutableState
                pure $ M.filterWithKey
                      (\depK _ -> S.member depK (jbDeps plan))
                      (stOutputs state)

              -- do the work
              newOutput <- fn filteredOutput (jbInput plan)

              -- update the state
              _ <- liftIO $ STM.atomically $ do
                STM.modifyTVar' mutableState (markJobAsDone k newOutput)
                STM.modifyTVar' inFlight (S.delete k)

              -- get the resulting jobs
              newReadyJobs <- liftIO getReadyJobsIO

              -- run them
              traverse_ (liftIO . Ki.fork scope . unsafePrimToIO . doJob) (M.toList newReadyJobs)

        -- start first jobs
        traverse_ (liftIO . Ki.fork scope . unsafePrimToIO . doJob) (M.toList readyJobs)

        -- wait for all the sillyness to stop
        liftIO $ STM.atomically $ Ki.awaitAll scope

        -- read the var and give up
        liftIO $ STM.readTVarIO mutableState

        -- get jobs available to start, fork them, and add key to `inFlight`
        -- each one, when done, updates state, and then checks again what can
        -- be started, and forks those
        -- when no more inputs (and nothing else in flight, return state)

