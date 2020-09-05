{-# LANGUAGE OverloadedStrings #-}

module Test.Prettier
  ( spec,
  )
where

import qualified Data.Map as M
import qualified Data.Text.IO as T
import Language.Mimsa.Printer
import Language.Mimsa.Types
import Test.Hspec

spec :: Spec
spec =
  describe "Prettier"
    $ describe "MonoType"
    $ do
      it "String" $
        T.putStrLn (prettyPrint MTString)
      it "Function" $
        T.putStrLn (prettyPrint $ MTFunction (MTFunction MTInt MTString) MTBool)
      it "Record" $
        T.putStrLn
          ( prettyPrint
              ( MTRecord $
                  M.fromList
                    [ (mkName "dog", MTUnit),
                      (mkName "horse", MTString),
                      (mkName "maybeDog", MTData (mkConstruct "Maybe") [MTString])
                    ]
              )
          )
      it "Pair" $
        T.putStrLn (prettyPrint $ MTPair (MTFunction MTInt MTInt) MTString)
      it "Variables" $
        T.putStrLn (prettyPrint $ MTFunction (MTVar $ NamedVar $ Name "catch") (MTVar $ NumberedVar 22))
