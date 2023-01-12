module Main
  ( main,
  )
where

import qualified Test.Backend.ESModulesJS
import qualified Test.Backend.Typescript
import Test.Hspec

main :: IO ()
main =
  hspec $ do
    Test.Backend.ESModulesJS.spec
    Test.Backend.Typescript.spec
