{-# LANGUAGE OverloadedStrings #-}

module Test.UnitTest
  ( spec,
  )
where

import Data.Either (isLeft)
import qualified Data.Set as S
import Language.Mimsa.Project.Helpers
import Language.Mimsa.Project.UnitTest
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.Project.UnitTest
import Language.Mimsa.Types.Store
import Test.Data.Project
import Test.Hspec
import Test.Utils.Helpers

getHashOfName :: Project ann -> Name -> ExprHash
getHashOfName prj name =
  case lookupBindingName prj name of
    Just a -> a
    _ -> error "could not getHashOfName"

spec :: Spec
spec =
  describe "UnitTest" $ do
    it "True is a valid test" $ do
      createUnitTest stdLib "True" (bool True) (TestName "True is true")
        `shouldBe` Right
          ( UnitTest
              (TestName "True is true")
              (TestSuccess True)
              (bool True)
              mempty
          )
    it "False is a valid (but failing) test" $ do
      createUnitTest stdLib "False" (bool False) (TestName "False is not true")
        `shouldBe` Right
          ( UnitTest
              (TestName "False is not true")
              (TestSuccess False)
              (bool False)
              mempty
          )
    it "100 is not a valid test" $ do
      createUnitTest stdLib "100" (int 100) (TestName "100 is not a valid test")
        `shouldSatisfy` isLeft
    it "Finds incrementInt and addInt" $ do
      let expr =
            MyInfix
              mempty
              Equals
              (int 1)
              (MyApp mempty (MyVar mempty (mkName "incrementInt")) (int 1))
      createUnitTest stdLib "1 == incrementInt(1)" expr (TestName "incrementInt is a no-op")
        `shouldBe` Right
          ( UnitTest
              (TestName "incrementInt is a no-op")
              (TestSuccess False)
              expr
              ( S.fromList
                  [ getHashOfName stdLib (mkName "incrementInt"),
                    getHashOfName stdLib (mkName "addInt")
                  ]
              )
          )
