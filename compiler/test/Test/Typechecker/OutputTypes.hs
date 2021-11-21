{-# LANGUAGE OverloadedStrings #-}

module Test.Typechecker.OutputTypes
  ( spec,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import Language.Mimsa.Actions.Shared
import qualified Language.Mimsa.Interpreter.UseSwaps as Swaps
import Language.Mimsa.Printer
import Language.Mimsa.Typechecker.OutputTypes
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Project.SourceItem
import Language.Mimsa.Types.Project.SourceSpan
import Language.Mimsa.Types.ResolvedExpression
import Language.Mimsa.Types.Swaps
import Language.Mimsa.Types.Typechecker
import Test.Data.Project
import Test.Hspec

unsafeTypecheckExpr ::
  Text ->
  ResolvedExpression Annotation
unsafeTypecheckExpr tx = case evaluateText testStdlib tx of
  Right a -> a
  Left e -> error (T.unpack (prettyPrint e))

useSwaps' :: Swaps -> Expr Variable ann -> Expr Name ann
useSwaps' swaps expr = case Swaps.useSwaps swaps expr of
  Right a -> a
  _ -> error "using swaps failed in OutputTypes test"

getExpressionSourceItems' :: Text -> Expr Name MonoType -> [SourceItem]
getExpressionSourceItems' = getExpressionSourceItems

spec :: Spec
spec = do
  describe "Output types" $ do
    it "Single type in literal" $ do
      getExpressionSourceItems'
        "True"
        ( MyLiteral
            (MTPrim (Location 1 4) MTBool)
            (MyBool True)
        )
        `shouldBe` [ SourceItem
                       "True :: Boolean"
                       ( SourceSpan
                           { ssRowStart = 1,
                             ssRowEnd = 1,
                             ssColStart = 2,
                             ssColEnd = 5
                           }
                       )
                   ]
    it "Includes pattern matches" $ do
      let expr =
            unsafeTypecheckExpr
              "\\a -> match a with (Just b) -> b | _ -> 0"

      let typedStoreExpr = useSwaps' (reSwaps expr) (reTypedExpression expr)
      getExpressionSourceItems (reInput expr) typedStoreExpr
        `shouldSatisfy` \a -> length a > 6
