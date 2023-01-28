{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Actions.Build
  ( spec,
  )
where

import Control.Monad.IO.Class
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Language.Mimsa.Actions.Helpers.Build as Actions
import Test.Hspec

job :: Actions.Job IO Int Text [Text]
job deps input =
  pure ([input] <> mconcat (M.elems deps))


doJobs :: Actions.Job IO Int Text [Text] -> Actions.State Int Text [Text] -> IO (Actions.State Int Text [Text])
doJobs thisJob state = do
  ioResponse <- liftIO $ Actions.doJobsIO thisJob state
  pureResponse <- liftIO $ Actions.doJobs thisJob state
  if ioResponse == pureResponse
      then pure ioResponse
      else error $ "Responses don't match: " <> show ioResponse <> " and " <> show pureResponse

spec :: Spec
spec = do
  describe "Build" $ do
    it "Empty state is a no-op" $ do
      let state = Actions.State mempty mempty
      newState <- liftIO $ doJobs job state
      newState `shouldBe` state
    it "Run job on single item" $ do
      let inputs = M.singleton 1 (Actions.Plan mempty "Hello")
      let state = Actions.State inputs mempty
      newState <- liftIO $ doJobs job state
      let expectedOutputs = M.singleton 1 ["Hello"]
      Actions.stOutputs newState `shouldBe` expectedOutputs
    it "Run job with a dep" $ do
      let inputs =
            M.fromList
              [ (1, Actions.Plan mempty "Hello"),
                (2, Actions.Plan (S.singleton 1) "Egg"),
                (3, Actions.Plan (S.singleton 1) "Horse"),
                (4, Actions.Plan (S.fromList [1, 3]) "Dog")
              ]
      let state = Actions.State inputs mempty
      let run = doJobs job
      newState <- liftIO $ run state
      let expectedOutputs =
            M.fromList
              [ (1, ["Hello"]),
                (2, ["Egg", "Hello"]),
                (3, ["Horse", "Hello"]),
                (4, ["Dog", "Hello", "Horse", "Hello"])
              ]
      Actions.stOutputs newState `shouldBe` expectedOutputs
    it "If all work is done, just return it" $ do
      let inputs =
            M.fromList
              [ (1, Actions.Plan mempty "Hello"),
                (2, Actions.Plan (S.singleton 1) "Egg"),
                (3, Actions.Plan (S.singleton 1) "Horse")
              ]
      let outputs =
            M.fromList
              [ (1, ["Hello!"]),
                (2, ["Egg!", "Hello!"]),
                (3, ["Horse!", "Hello!"])
              ]
      let state = Actions.State inputs outputs
      let run = doJobs job
      newState <- liftIO $ run state
      Actions.stOutputs newState `shouldBe` outputs
    it "If outputs already exist, uses them instead of calculating" $ do
      let inputs =
            M.fromList
              [ (1, Actions.Plan mempty "Hello"),
                (2, Actions.Plan (S.singleton 1) "Egg"),
                (3, Actions.Plan (S.singleton 1) "Horse"),
                (4, Actions.Plan (S.fromList [1, 3]) "Dog")
              ]
      let outputs =
            M.fromList
              [ (1, ["Hello!"]),
                (2, ["Egg!", "Hello!"]),
                (3, ["Horse!", "Hello!"])
              ]
      let state = Actions.State inputs outputs
      let run = doJobs job
      newState <- liftIO $ run state
      let expectedOutputs =
            outputs
              <> M.fromList
                [ (4, ["Dog", "Hello!", "Horse!", "Hello!"])
                ]
      Actions.stOutputs newState `shouldBe` expectedOutputs
    it "Detects missing deps" $ do
      let inputs =
            M.fromList
              [ ( 1,
                  Actions.Plan (S.singleton (100 :: Int)) ("100 doesn't exist" :: String)
                ),
                (2, Actions.Plan (S.fromList [101, 1]) "101 doesn't exist either")
              ]
          outputs = mempty
          state = Actions.State inputs outputs
      Actions.getMissing state `shouldBe` S.fromList [100, 101]
