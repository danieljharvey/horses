{-# LANGUAGE OverloadedStrings #-}

module Test.Typechecker.OutputTypes
  ( spec,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import Language.Mimsa.Actions.Shared
import Language.Mimsa.Printer
import Language.Mimsa.Typechecker.OutputTypes
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Project.SourceItem
import Language.Mimsa.Types.Project.SourceSpan
import Language.Mimsa.Types.ResolvedExpression
import Language.Mimsa.Types.Typechecker
import Test.Data.Project
import Test.Hspec

unsafeTypecheckExpr ::
  Text ->
  ResolvedExpression Annotation
unsafeTypecheckExpr tx = case evaluateText testStdlib tx of
  Right a -> a
  Left e -> error (T.unpack (prettyPrint e))

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
    it "Includes pattern matches" $ do
      let expr =
            unsafeTypecheckExpr
              "\\a -> match a with (Just b) -> b | _ -> 0"
      getExpressionSourceItems (reInput expr) (reTypedExpression expr)
        `shouldSatisfy` \a -> length a > 6
