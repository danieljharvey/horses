{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Repl
  ( spec,
  )
where

import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Language.Mimsa.Interpreter
import Language.Mimsa.Repl
import Language.Mimsa.Syntax
import Language.Mimsa.Types
import Test.Helpers
import Test.Hspec

fstExpr :: StoreExpression
fstExpr =
  unsafeGetExpr "\\tuple -> let (a,b) = tuple in a"

sndExpr :: StoreExpression
sndExpr = unsafeGetExpr "\\tuple -> let (a,b) = tuple in b"

isTenExpr :: StoreExpression
isTenExpr =
  unsafeGetExpr "\\i -> if eqInt(i)(10) then Right i else Left i"

eqTenExpr :: StoreExpression
eqTenExpr =
  unsafeGetExpr "\\i -> eqInt(10)(i)"

fmapSum :: StoreExpression
fmapSum =
  unsafeGetExpr
    "\\f -> \\a -> case a of Left (\\l -> Left l) | Right (\\r -> Right f(r))"

listUncons :: StoreExpression
listUncons =
  unsafeGetExpr "\\myList -> let [head,tail] = myList in (head,tail)"

listHead :: StoreExpression
listHead =
  unsafeGetExpr' "\\i -> compose(fst)(listUncons)(i)" $ -- why does this work but not compose(fst)(listUncons)
    Bindings
      ( M.fromList
          [ (mkName "compose", ExprHash 6),
            (mkName "fst", ExprHash 1),
            (mkName "listUncons", ExprHash 5)
          ]
      )

listTail :: StoreExpression
listTail =
  unsafeGetExpr' "compose(snd)(listUncons)" $
    Bindings
      ( M.fromList
          [ (mkName "compose", ExprHash 6),
            (mkName "snd", ExprHash 7),
            (mkName "listUncons", ExprHash 5)
          ]
      )

compose :: StoreExpression
compose =
  unsafeGetExpr "\\f -> \\g -> \\a -> f(g(a))"

list :: StoreExpression
list = unsafeGetExpr' "{ head: listHead, tail: listTail }" bindings'
  where
    bindings' =
      Bindings
        ( M.fromList
            [ (mkName "listHead", ExprHash 8),
              (mkName "listTail", ExprHash 9)
            ]
        )

idExpr :: StoreExpression
idExpr = unsafeGetExpr "\\i -> i"

constExpr :: StoreExpression
constExpr = unsafeGetExpr "\\a -> \\b -> a"

stdLib :: StoreEnv
stdLib = StoreEnv store' bindings'
  where
    store' =
      Store $
        M.fromList
          [ (ExprHash 1, fstExpr),
            (ExprHash 2, isTenExpr),
            (ExprHash 3, eqTenExpr),
            (ExprHash 4, fmapSum),
            (ExprHash 5, listUncons),
            (ExprHash 6, compose),
            (ExprHash 7, sndExpr),
            (ExprHash 8, listHead),
            (ExprHash 9, listTail),
            (ExprHash 10, list),
            (ExprHash 11, idExpr),
            (ExprHash 12, constExpr)
          ]
    bindings' =
      Bindings $
        M.fromList
          [ (mkName "fst", ExprHash 1),
            (mkName "isTen", ExprHash 2),
            (mkName "eqTen", ExprHash 3),
            (mkName "fmapSum", ExprHash 4),
            (mkName "listUncons", ExprHash 5),
            (mkName "compose", ExprHash 6),
            (mkName "snd", ExprHash 7),
            (mkName "listHead", ExprHash 8),
            (mkName "listTail", ExprHash 9),
            (mkName "list", ExprHash 10),
            (mkName "id", ExprHash 11),
            (mkName "const", ExprHash 12)
          ]

unsafeGetExpr' :: Text -> Bindings -> StoreExpression
unsafeGetExpr' input bindings' =
  case parseExpr input of
    Right expr' -> StoreExpression bindings' expr'
    a -> error $ "Error evaluating " <> T.unpack input <> ": " <> show a

unsafeGetExpr :: Text -> StoreExpression
unsafeGetExpr input = unsafeGetExpr' input mempty

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
      it "let x = ((1,2)) in fst(x)" $ do
        result <- eval stdLib "let x = ((1,2)) in fst(x)"
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
      it "case isTen(9) of Left (\\l -> \"It's not ten\") | Right (\\r -> \"It's ten!\")" $ do
        result <- eval stdLib "case isTen(9) of Left (\\l -> \"It's not ten\") | Right (\\r -> \"It's ten!\")"
        result
          `shouldBe` Right
            (MTString, str' "It's not ten")
        result2 <- eval stdLib "case isTen(10) of Left (\\l -> \"It's not ten\") | Right (\\r -> \"It's ten!\")"
        result2
          `shouldBe` Right
            (MTString, str' "It's ten!")
      it "case (Left 1) of Left (\\l -> Left l) | Right (\\r -> Right r)" $ do
        result <- eval stdLib "case (Left 1) of Left (\\l -> Left l) | Right (\\r -> Right r)"
        result
          `shouldBe` Right
            ( (MTSum MTInt (MTVar (mkName "U1"))),
              ( MySum MyLeft (int 1)
              )
            )
      it "\\sum -> case sum of Left (\\l -> Left l) | Right (\\r -> Right eqTen(r))" $ do
        result <- eval stdLib "\\sum -> case sum of Left (\\l -> Left l) | Right (\\r -> Right eqTen(r))"
        result
          `shouldBe` Right
            ( MTFunction
                (MTSum (MTVar (mkName "U10")) MTInt)
                (MTSum (MTVar (mkName "U10")) MTBool),
              ( MyLambda
                  (mkName "sum")
                  ( MyCase
                      (MyVar (mkName "sum"))
                      ( MyLambda
                          (mkName "l")
                          (MySum MyLeft (MyVar (mkName "l")))
                      )
                      ( MyLambda
                          (mkName "r")
                          ( MySum
                              MyRight
                              ( MyApp
                                  (MyVar (mkName "var0"))
                                  (MyVar (mkName "r"))
                              )
                          )
                      )
                  )
              )
            )
      it "let sum = (Left 1) in ((fmapSum eqTen) sum)" $ do
        result <- eval stdLib "let sum = (Left 1) in fmapSum (eqTen) (sum)"
        result
          `shouldBe` Right
            (MTSum MTInt MTBool, MySum MyLeft (int 1))
      it "let [head, tail] = ([1,2,3]) in head" $ do
        result <- eval stdLib "let [head, tail] = ([1,2,3]) in head"
        result
          `shouldBe` Right (MTInt, int 1)
      it "let [head, tail] = ([1,2,3]) in tail" $ do
        result <- eval stdLib "let [head, tail] = ([1,2,3]) in tail"
        result
          `shouldBe` Right
            ( MTSum MTUnit (MTList MTInt),
              MySum MyRight $ MyList $ NE.fromList [int 2, int 3]
            )
      it "listUncons([1,2,3])" $ do
        result <- eval stdLib "listUncons([1,2,3])"
        result
          `shouldBe` Right
            ( MTPair MTInt (MTSum MTUnit (MTList MTInt)),
              MyPair
                (int 1)
                ( MySum
                    MyRight
                    ( MyList $
                        NE.fromList [int 2, int 3]
                    )
                )
            )
      it "let listHead = (compose(fst)(listUncons)) in listHead([1,2,3])" $ do
        result <- eval stdLib "let listHead = (compose(fst)(listUncons)) in listHead([1,2,3])"
        result `shouldBe` Right (MTInt, int 1)
      it "let good = { dog: True } in good.dog" $ do
        result <- eval stdLib "let good = ({ dog: True }) in good.dog"
        result `shouldBe` Right (MTBool, bool True)
      it "let prelude = { id: (\\i -> i) } in prelude.id" $ do
        result <- eval stdLib "let prelude = ({ id: (\\i -> i) }) in prelude.id"
        result
          `shouldBe` Right
            ( MTFunction (MTVar (mkName "U1")) (MTVar (mkName "U1")),
              MyLambda (mkName "i") (MyVar (mkName "i"))
            )
      it "let prelude = ({ id: (\\i -> i) }) in prelude.id(1)" $ do
        result <- eval stdLib "let prelude = ({ id: (\\i -> i) }) in prelude.id(1)"
        result
          `shouldBe` Right
            ( MTInt,
              int 1
            )
      it "let bigPrelude = ({ prelude: { id: (\\i -> i) } }) in bigPrelude.prelude.id(1)" $ do
        result <- eval stdLib "let bigPrelude = ({ prelude: { id: (\\i -> i) } }) in bigPrelude.prelude.id(1)"
        result
          `shouldBe` Right
            ( MTInt,
              int 1
            )
      it "listHead([1])" $ do
        result <- eval stdLib "listHead([1])"
        result `shouldBe` Right (MTInt, int 1)
      it "list.head([1])" $ do
        result <- eval stdLib "list.head([1])"
        result `shouldBe` Right (MTInt, int 1)
      it "listTail([1])" $ do
        result <- eval stdLib "listTail([1])"
        result `shouldBe` Right (MTSum MTUnit (MTList MTInt), MySum MyLeft (MyLiteral MyUnit))
      it "list.tail([1])" $ do
        result <- eval stdLib "list.tail([1])"
        result `shouldBe` Right (MTSum MTUnit (MTList MTInt), MySum MyLeft (MyLiteral MyUnit))
      it "let reuse = ({ first: id(1), second: id(2) }) in reuse.first" $ do
        result <- eval stdLib "let reuse = ({ first: id(1), second: id(2) }) in reuse.first"
        result `shouldBe` Right (MTInt, int 1)
      it "let const2 = \\a -> \\b -> a in (let reuse = ({ first: const2(1), second: const2(True) }) in reuse.first(100))" $ do
        result <- eval stdLib "let const2 = \\a -> \\b -> a in (let reuse = ({ first: const2(1), second: const2(True) }) in reuse.first(100))"
        result `shouldBe` Right (MTInt, int 1)
      it "let const2 = \\a -> \\b -> a in (let reuse = ({ first: const2(True), second: const2(2) }) in reuse.second(100))" $ do
        result <- eval stdLib "let const2 = \\a -> \\b -> a in (let reuse = ({ first: const2(True), second: const2(2) }) in reuse.second(100))"
        result `shouldBe` Right (MTInt, int 2)
