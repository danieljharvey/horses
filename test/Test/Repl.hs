{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Repl
  ( spec,
  )
where

import qualified Data.Map as M
import Data.Text (Text)
import Language.Mimsa.Interpreter
import Language.Mimsa.Repl
import Language.Mimsa.Types
import Test.Helpers
import Test.Hspec

fstExpr :: Expr
fstExpr =
  ( MyLambda
      (mkName "tuple")
      ( MyLetPair
          (mkName "a")
          (mkName "b")
          (MyVar (mkName "tuple"))
          (MyVar (mkName "a"))
      )
  )

fstStoreExpr :: StoreExpression
fstStoreExpr = StoreExpression mempty fstExpr

stdLib :: StoreEnv
stdLib = StoreEnv store' bindings'
  where
    store' = Store (M.singleton (ExprHash 1) fstStoreExpr)
    bindings' = Bindings (M.singleton (mkName "fst") (ExprHash 1))

eval :: StoreEnv -> Text -> IO (Either Text (MonoType, Expr))
eval env input =
  case evaluateText env input of
    Right (mt, expr', scope') -> do
      endExpr <- interpret scope' expr'
      case endExpr of
        Right a -> pure (Right (mt, a))
        Left e -> pure (Left e)
    Left e -> pure (Left e)

spec :: Spec
spec = do
  describe "Repl" $ do
    describe "End to end parsing to evaluation" $ do
      it "let x = ((1,2)) in (fst x)" $ do
        result <- eval stdLib "let x = ((1,2)) in (fst x)"
        result
          `shouldBe` Right
            (MTInt, int 1)
      it "\\x -> if x then Left 1 else Right \"yes\"" $ do
        result <- eval stdLib "\\x -> if x then Right \"yes\" else Left 1"
        result
          `shouldBe` Right
            ( MTFunction MTBool (MTSum MTInt MTString),
              ( MyLambda
                  (mkName "x")
                  ( MyIf
                      (MyVar (mkName "x"))
                      (MySum MyRight (str' "yes"))
                      (MySum MyLeft (int 1))
                  )
              )
            )
