{-# LANGUAGE OverloadedStrings #-}

module Test.Modules.ToStoreExprs
  ( spec,
  )
where

import Data.Functor
import qualified Data.Map.Strict as M
import Language.Mimsa.Modules.HashModule
import Language.Mimsa.Modules.Parse
import Language.Mimsa.Modules.ToStoreExprs
import Language.Mimsa.Modules.Typecheck
import Language.Mimsa.Core
import Language.Mimsa.Store
import Language.Mimsa.Core
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Modules.DefIdentifier
import Language.Mimsa.Types.Modules.Module
import Language.Mimsa.Types.Store
import Language.Mimsa.Types.Typechecker
import Test.Hspec
import Test.Utils.Helpers

toStoreExpressions' :: Module Annotation -> CompiledModule Annotation
toStoreExpressions' mod' =
  let action :: Either (Error Annotation) (CompiledModule Annotation)
      action = do
        tcMods <- typecheckAllModules mempty (prettyPrint mod') mod'
        case M.lookup (snd $ serializeModule mod') tcMods of
          Just tcMod -> (fmap . fmap) getAnnotationForType (toStoreExpressions tcMods tcMod)
          Nothing -> error "Could not find the module we just typechecked"
   in fromRight action

spec :: Spec
spec = do
  describe "Compile modules" $ do
    it "Empty module, no outputs" $ do
      toStoreExpressions' mempty
        `shouldBe` CompiledModule
          { cmStore = mempty,
            cmExprs = mempty
          }
    it "Single expression, single output with no deps" $ do
      let expr = unsafeParseExpr "\\a -> a" $> mempty
          storeExpr = StoreExpression expr mempty mempty mempty mempty
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
      toStoreExpressions' inputModule `shouldBe` expected
    it "Two expressions, one depends on the other" $ do
      let exprA = unsafeParseExpr "\\a -> a" $> mempty
          storeExprA = StoreExpression exprA mempty mempty mempty mempty
          hashA = getStoreExpressionHash storeExprA

          exprB = unsafeParseExpr "id 100" $> mempty
          storeExprB = StoreExpression exprB (M.singleton (Nothing, "id") hashA) mempty mempty mempty
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
      toStoreExpressions' inputModule `shouldBe` expected

    it "Two expressions, one type, second expression uses type indirectly and should have it as a dep" $ do
      let lookupInCompiled name compiled =
            fromJust $ M.lookup (DIName name) (cmExprs compiled) >>= \hash -> M.lookup hash (getStore (cmStore compiled))

      let inputModule =
            fromRight $
              parseModule mempty $
                joinLines
                  [ "export type Either e a = Left e | Right a",
                    "def useEither val = match val with Right a -> a | _ -> False",
                    "def shouldHaveEitherAsDep val = useEither val"
                  ]
          output = toStoreExpressions' inputModule
      -- three output items
      cmStore output `shouldSatisfy` \(Store a) -> M.size a == 3
      -- main one has Either as dep
      let shouldHaveEither = lookupInCompiled "shouldHaveEitherAsDep" output
      storeTypes shouldHaveEither `shouldSatisfy` \a -> M.size a == 1
