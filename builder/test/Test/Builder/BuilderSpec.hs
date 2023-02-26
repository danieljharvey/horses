{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Builder.BuilderSpec
  ( spec,
  )
where

import qualified Builder
import Control.Monad.IO.Class
import Control.Monad.Identity
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text (Text)
import Test.Hspec

job :: (Applicative m) => Builder.Job m Error Int Text [Text]
job deps input =
  pure $ Right ([input] <> mconcat (M.elems deps))

-- error on Egg
jobWithError :: (Applicative m) => Builder.Job m Error Int Text [Text]
jobWithError _ "Egg" =
  pure (Left OhNo)
jobWithError deps input =
  pure $ Right ([input] <> mconcat (M.elems deps))

data Error = OhNo
  deriving (Eq, Ord, Show)

doJobs ::
  Builder.Job Identity Error Int Text [Text] ->
  Builder.State Int Text [Text] ->
  IO (Either Error (Builder.State Int Text [Text]))
doJobs thisJob state = do
  let ioJob a b =
        pure (runIdentity (thisJob a b))
  ioResponse <- liftIO $ Builder.doJobsIO ioJob state
  let pureResponse = Builder.doJobsPure thisJob state
  if ioResponse == pureResponse
    then pure ioResponse
    else error $ "Responses don't match: " <> show ioResponse <> " and " <> show pureResponse

spec :: Spec
spec = do
  describe "Build" $ do
    it "Empty state is a no-op" $ do
      let state = Builder.State mempty mempty
      newState <- liftIO $ doJobs job state
      newState `shouldBe` Right state
    it "Run job on single item" $ do
      let inputs = M.singleton 1 (Builder.Plan mempty "Hello")
      let state = Builder.State inputs mempty
      newState <- liftIO $ doJobs job state
      let expectedOutputs = M.singleton 1 ["Hello"]
      Builder.stOutputs <$> newState `shouldBe` Right expectedOutputs
    it "Run job with a dep" $ do
      let inputs =
            M.fromList
              [ (1, Builder.Plan mempty "Hello"),
                (2, Builder.Plan (S.singleton 1) "Egg"),
                (3, Builder.Plan (S.singleton 1) "Horse"),
                (4, Builder.Plan (S.fromList [1, 3]) "Dog")
              ]
      let state = Builder.State inputs mempty
      let run = doJobs job
      newState <- liftIO $ run state
      let expectedOutputs =
            M.fromList
              [ (1, ["Hello"]),
                (2, ["Egg", "Hello"]),
                (3, ["Horse", "Hello"]),
                (4, ["Dog", "Hello", "Horse", "Hello"])
              ]
      Builder.stOutputs <$> newState `shouldBe` Right expectedOutputs
    it "Run job with a dep that 'throws'" $ do
      let inputs =
            M.fromList
              [ (1, Builder.Plan mempty "Hello"),
                (2, Builder.Plan (S.singleton 1) "Egg"),
                (3, Builder.Plan (S.singleton 1) "Horse"),
                (4, Builder.Plan (S.fromList [1, 3]) "Dog")
              ]
      let state = Builder.State inputs mempty
      let run = doJobs jobWithError
      newState <- liftIO $ run state
      Builder.stOutputs <$> newState `shouldBe` Left OhNo

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
      let run = doJobs job
      newState <- liftIO $ run state
      Builder.stOutputs <$> newState `shouldBe` Right outputs
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
      let run = doJobs job
      newState <- liftIO $ run state
      let expectedOutputs =
            outputs
              <> M.fromList
                [ (4, ["Dog", "Hello!", "Horse!", "Hello!"])
                ]
      Builder.stOutputs <$> newState `shouldBe` Right expectedOutputs
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
