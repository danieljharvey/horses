{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.DepGraph
  ( spec,
  )
where

import qualified Data.Text.IO as T
import Language.Mimsa.Store.DepGraph
import Language.Mimsa.Types
import Test.Hspec
import Test.StoreData

spec :: Spec
spec =
  describe "DepGraph"
    $ it "list"
    $ do
      let result = createDepGraph (mkName "list") (store stdLib) list
      T.putStrLn (prettyPrint result)
      True `shouldBe` True
