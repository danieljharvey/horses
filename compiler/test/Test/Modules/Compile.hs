{-# LANGUAGE OverloadedStrings #-}

module Test.Modules.Compile
  ( spec,
  )
where

import Data.Functor
import qualified Data.Map as M
import Language.Mimsa.Modules.Check
import Language.Mimsa.Modules.Compile
import Language.Mimsa.Modules.HashModule
import Language.Mimsa.Modules.Monad
import Language.Mimsa.Printer
import Language.Mimsa.Store
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Modules.DefIdentifier
import Language.Mimsa.Types.Modules.Module
import Language.Mimsa.Types.Store
import Test.Hspec
import Test.Utils.Helpers

compile' :: Module Annotation -> CompiledModule Annotation
compile' mod' =
  let action = do
        tcMods <- typecheckAllModules mod'
        case M.lookup (hashModule mod') tcMods of
          Just tcMod -> compile tcMods tcMod
          Nothing -> error "Could not find the module we just typechecked"
   in fromRight $ runCheck (prettyPrint mod') mempty action

spec :: Spec
spec = do
  describe "Compile modules" $ do
    it "Empty module, no outputs" $ do
      compile' mempty
        `shouldBe` CompiledModule
          { cmStore = mempty,
            cmExprs = mempty
          }
    it "Single expression, single output with no deps" $ do
      let expr = unsafeParseExpr "\\a -> a" $> mempty
          storeExpr = StoreExpression expr mempty mempty mempty
          hash = getStoreExpressionHash storeExpr
          inputModule =
            mempty
              { moExpressions = M.singleton (DIName "id") expr
              }
          expected =
            CompiledModule
              { cmStore = Store $ M.singleton hash storeExpr,
                cmExprs = M.singleton (DIName "id") hash
              }
      compile' inputModule `shouldBe` expected
    it "Two expressions, one depends on the other" $ do
      let exprA = unsafeParseExpr "\\a -> a" $> mempty
          storeExprA = StoreExpression exprA mempty mempty mempty
          hashA = getStoreExpressionHash storeExprA

          exprB = unsafeParseExpr "id 100" $> mempty
          storeExprB = StoreExpression exprB (M.singleton (Nothing, "id") hashA) mempty mempty
          hashB = getStoreExpressionHash storeExprB

          inputModule =
            mempty
              { moExpressions =
                  M.fromList
                    [ (DIName "id", exprA),
                      (DIName "useId", exprB)
                    ]
              }
          expected =
            CompiledModule
              { cmStore =
                  Store $
                    M.fromList
                      [ (hashA, storeExprA),
                        (hashB, storeExprB)
                      ],
                cmExprs =
                  M.fromList
                    [ (DIName "id", hashA),
                      (DIName "useId", hashB)
                    ]
              }
      compile' inputModule `shouldBe` expected
