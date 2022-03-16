{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Actions.Build
  ( spec,
  )
where

import Control.Monad.IO.Class
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Language.Mimsa.Actions.Helpers.Build as Actions
import Test.Hspec

job :: Actions.Job IO Int Text [Text]
job deps input =
  pure ([input] <> mconcat (M.elems deps))

spec :: Spec
spec = do
  describe "Build" $ do
    it "Empty state is a no-op" $ do
      let state = Actions.State mempty mempty
      newState <- liftIO $ Actions.doJobs job state
      newState `shouldBe` state
    it "Run job on single item" $ do
      let inputs = M.singleton 1 (Actions.Plan mempty "Hello")
      let state = Actions.State inputs mempty
      newState <- liftIO $ Actions.doJobs job state
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
      let run = Actions.doJobs job
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
      let run = Actions.doJobs job
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
      let run = Actions.doJobs job
      newState <- liftIO $ run state
      let expectedOutputs =
            outputs
              <> M.fromList
                [ (4, ["Dog", "Hello!", "Horse!", "Hello!"])
                ]
      Actions.stOutputs newState `shouldBe` expectedOutputs
