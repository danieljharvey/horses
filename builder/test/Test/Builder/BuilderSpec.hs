{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Builder.BuilderSpec
  ( spec,
  )
where

import qualified Builder
import Control.Monad.IO.Class
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text (Text)
import Test.Hspec

job :: Builder.Job IO Int Text [Text]
job deps input =
  pure ([input] <> mconcat (M.elems deps))

spec :: Spec
spec = do
  describe "Build" $ do
    it "Empty state is a no-op" $ do
      let state = Builder.State mempty mempty
      newState <- liftIO $ Builder.doJobs job state
      newState `shouldBe` state
    it "Run job on single item" $ do
      let inputs = M.singleton 1 (Builder.Plan mempty "Hello")
      let state = Builder.State inputs mempty
      newState <- liftIO $ Builder.doJobs job state
      let expectedOutputs = M.singleton 1 ["Hello"]
      Builder.stOutputs newState `shouldBe` expectedOutputs
    it "Run job with a dep" $ do
      let inputs =
            M.fromList
              [ (1, Builder.Plan mempty "Hello"),
                (2, Builder.Plan (S.singleton 1) "Egg"),
                (3, Builder.Plan (S.singleton 1) "Horse"),
                (4, Builder.Plan (S.fromList [1, 3]) "Dog")
              ]
      let state = Builder.State inputs mempty
      let run = Builder.doJobs job
      newState <- liftIO $ run state
      let expectedOutputs =
            M.fromList
              [ (1, ["Hello"]),
                (2, ["Egg", "Hello"]),
                (3, ["Horse", "Hello"]),
                (4, ["Dog", "Hello", "Horse", "Hello"])
              ]
      Builder.stOutputs newState `shouldBe` expectedOutputs
    it "If all work is done, just return it" $ do
      let inputs =
            M.fromList
              [ (1, Builder.Plan mempty "Hello"),
                (2, Builder.Plan (S.singleton 1) "Egg"),
                (3, Builder.Plan (S.singleton 1) "Horse")
              ]
      let outputs =
            M.fromList
              [ (1, ["Hello!"]),
                (2, ["Egg!", "Hello!"]),
                (3, ["Horse!", "Hello!"])
              ]
      let state = Builder.State inputs outputs
      let run = Builder.doJobs job
      newState <- liftIO $ run state
      Builder.stOutputs newState `shouldBe` outputs
    it "If outputs already exist, uses them instead of calculating" $ do
      let inputs =
            M.fromList
              [ (1, Builder.Plan mempty "Hello"),
                (2, Builder.Plan (S.singleton 1) "Egg"),
                (3, Builder.Plan (S.singleton 1) "Horse"),
                (4, Builder.Plan (S.fromList [1, 3]) "Dog")
              ]
      let outputs =
            M.fromList
              [ (1, ["Hello!"]),
                (2, ["Egg!", "Hello!"]),
                (3, ["Horse!", "Hello!"])
              ]
      let state = Builder.State inputs outputs
      let run = Builder.doJobs job
      newState <- liftIO $ run state
      let expectedOutputs =
            outputs
              <> M.fromList
                [ (4, ["Dog", "Hello!", "Horse!", "Hello!"])
                ]
      Builder.stOutputs newState `shouldBe` expectedOutputs
    it "Detects missing deps" $ do
      let inputs =
            M.fromList
              [ ( 1,
                  Builder.Plan (S.singleton (100 :: Int)) ("100 doesn't exist" :: String)
                ),
                (2, Builder.Plan (S.fromList [101, 1]) "101 doesn't exist either")
              ]
          outputs = mempty
          state = Builder.State inputs outputs
      Builder.getMissing state `shouldBe` S.fromList [100, 101]
