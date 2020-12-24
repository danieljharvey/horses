{-# LANGUAGE OverloadedStrings #-}

module Test.Prettier
  ( spec,
  )
where

import qualified Data.Map as M
import qualified Data.Text.IO as T
import Language.Mimsa.Printer
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Typechecker
import Test.Hspec

spec :: Spec
spec =
  describe "Prettier"
    $ describe "MonoType"
    $ do
      it "String" $
        T.putStrLn (prettyPrint MTString)
      it "Function" $
        let mt :: MonoType
            mt =
              MTFunction
                mempty
                (MTFunction mempty (MTPrim mempty MTInt) (MTPrim mempty MTString))
                (MTPrim mempty MTBool)
         in T.putStrLn
              ( prettyPrint mt
              )
      it "Record" $
        let mt :: MonoType
            mt =
              MTRecord mempty $
                M.fromList
                  [ (mkName "dog", MTPrim mempty MTUnit),
                    (mkName "horse", MTPrim mempty MTString),
                    ( mkName "maybeDog",
                      MTData
                        mempty
                        (mkTyCon "Maybe")
                        [MTPrim mempty MTString]
                    )
                  ]
         in T.putStrLn
              ( prettyPrint mt
              )
      it "Pair" $
        let mt :: MonoType
            mt =
              MTPair
                mempty
                (MTFunction mempty (MTPrim mempty MTInt) (MTPrim mempty MTInt))
                (MTPrim mempty MTString)
         in T.putStrLn
              (prettyPrint mt)
      it "Variables" $
        let mt :: MonoType
            mt =
              MTFunction
                mempty
                ( MTVar mempty
                    $ NamedVar
                    $ Name "catch"
                )
                (MTVar mempty $ NumberedVar 22)
         in T.putStrLn
              ( prettyPrint mt
              )
      it "Names type vars" $ do
        let mt = MTVar () (NumberedVar 1)
        prettyPrint mt `shouldBe` "A"
      it "Names type vars 2" $ do
        let mt = MTVar () (NumberedVar 26)
        prettyPrint mt `shouldBe` "Z"
      it "Names type vars 3" $ do
        let mt = MTVar () (NumberedVar 27)
        prettyPrint mt `shouldBe` "A1"
