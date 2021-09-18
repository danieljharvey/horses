{-# LANGUAGE OverloadedStrings #-}

module Test.Typechecker.OutputTypes
  ( spec,
  )
where

import Language.Mimsa.Typechecker.OutputTypes
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Project.SourceItem
import Language.Mimsa.Types.Project.SourceSpan
import Language.Mimsa.Types.Typechecker
import Test.Hspec

spec :: Spec
spec = do
  describe "Output types" $ do
    it "Single type in literal" $ do
      getExpressionSourceItems
        "True"
        ( MyLiteral
            (MTPrim (Location 1 4) MTBool, Location 1 4)
            (MyBool True)
        )
        `shouldBe` [ SourceItem
                       "Boolean"
                       ( SourceSpan
                           { ssRowStart = 1,
                             ssRowEnd = 1,
                             ssColStart = 1,
                             ssColEnd = 4
                           }
                       )
                   ]
