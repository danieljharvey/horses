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
        T.putStrLn
          ( prettyPrint $
              MTFunction
                (MTFunction (MTPrim MTInt) (MTPrim MTString))
                (MTPrim MTBool)
          )
      it "Record" $
        T.putStrLn
          ( prettyPrint
              ( MTRecord $
                  M.fromList
                    [ (mkName "dog", MTPrim MTUnit),
                      (mkName "horse", MTPrim MTString),
                      (mkName "maybeDog", MTData (mkTyCon "Maybe") [MTPrim MTString])
                    ]
              )
          )
      it "Pair" $
        T.putStrLn
          ( prettyPrint $
              MTPair
                (MTFunction (MTPrim MTInt) (MTPrim MTInt))
                (MTPrim MTString)
          )
      it "Variables" $
        T.putStrLn
          ( prettyPrint $
              MTFunction
                ( MTVar
                    $ NamedVar
                    $ Name "catch"
                )
                (MTVar $ NumberedVar 22)
          )
