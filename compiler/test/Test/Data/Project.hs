{-# LANGUAGE OverloadedStrings #-}

module Test.Data.Project
  ( testStdlib,
    idExpr,
    addBinding,
    addExprBinding,
  )
where

import Data.Functor
import Data.Text (Text)
import qualified Data.Text as T
import qualified Language.Mimsa.Actions.Monad as Actions
import qualified Language.Mimsa.Actions.Typecheck as Actions
import Language.Mimsa.Parser (parseExpr)
import Language.Mimsa.Printer
import Language.Mimsa.Project.Helpers
import Language.Mimsa.Project.Stdlib
import Language.Mimsa.Store
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.ResolvedExpression
import Language.Mimsa.Types.Store

-- polymorphic export for use in other tests
idExpr :: (Monoid ann) => StoreExpression ann
idExpr = idExpr' $> mempty
  where
    idExpr' =
      unsafeGetExpr "\\i -> i"

-- check removing annotation doesn't break stuff
testStdlib :: Project Annotation
testStdlib = case buildTestStdlib of
  Right stdLib' -> (stdLib' $> ()) $> mempty
  Left e ->
    error (T.unpack $ prettyPrint e)

buildTestStdlib :: Either (Error Annotation) (Project Annotation)
buildTestStdlib =
  Actions.run mempty action
    >>= \(proj, _, _) -> pure proj
  where
    action = do
      stdlibAction
      testStdlibAction

testStdlibAction :: Actions.ActionM ()
testStdlibAction = do
  addBinding
    "eq"
    "\\a -> \\b -> a == b"
  addBinding
    "eqTen"
    "\\i -> eq 10 i"
  addBinding
    "addInt"
    "\\a -> \\b -> a + b"
  addBinding
    "subtractInt"
    "\\a -> \\b -> a - b"
  addBinding
    "int"
    "{ add: addInt, subtract: subtractInt }"
  addBinding
    "incrementInt"
    "\\a -> addInt 1 a"
  addBinding
    "typeThese"
    "type These a b = This a | That b | These a b in {}"
  addBinding
    "aPair"
    "(1,2)"
  addBinding
    "aRecord"
    "{ a: 1, b: \"dog\" }"
  addBinding
    "typePerson"
    "type Person = Person { name: String, age: Int } in {}"
  addBinding
    "stringReduce"
    "let stringReduce = \\f -> \\def -> \\str -> match str with \"\" -> def | head ++ tail -> stringReduce f (f def head) tail; stringReduce"
  addListMonad
  addBinding
    "storeName"
    "\\newName -> let sas = (\\s -> (newName ++ \"!!!\", cons newName s)) in State sas"
  addType "type Pair a b = Pair a b"
  addType
    "type TrafficLight = Red | Yellow | Green"
  addIdentity
  addMonoid
  addMonoPair
  addTree
  addPropertyTests

addPropertyTests :: Actions.ActionM ()
addPropertyTests = do
  addBinding
    "constTrue"
    "\\(a: Boolean) -> True"
  addBinding
    "constFalse"
    "\\(a: Boolean) -> False"
  addBinding
    "invertTreeTwice"
    "\\(tree: Tree Int) -> invertTree (invertTree tree) == tree"

addListMonad :: Actions.ActionM ()
addListMonad = do
  addType
    "type List a = Cons a (List a) | Nil"
  addBinding
    "cons"
    "\\a -> \\list -> Cons a list"
  addBinding
    "nil"
    "Nil"

addMonoPair :: Actions.ActionM ()
addMonoPair = do
  addType
    "type MonoPair a = MonoPair a a"

addIdentity :: Actions.ActionM ()
addIdentity = addType "type Ident a = Ident a"

addMonoid :: Actions.ActionM ()
addMonoid = do
  addType
    "type Monoid a = Monoid (a -> a -> a) a"
  addBinding
    "stringMonoid"
    "Monoid (\\a -> \\b -> a ++ b) \"\""
  addBinding
    "sumMonoid"
    "Monoid (\\a -> \\b -> a + b) 0"
  addBinding
    "maybeMonoid"
    ( mconcat
        [ "\\innerM -> ",
          "Monoid (\\a -> \\b -> match (a,b) with ",
          "          (Just iA, Just iB) -> let (Monoid innerMappend innerMempty) = innerM; Just (innerMappend iA iB)",
          "        | (Just iA, Nothing) -> (Just iA) ",
          "        | (Nothing, Just iB) -> (Just iB) ",
          "        | _ -> Nothing) Nothing"
        ]
    )

addTree :: Actions.ActionM ()
addTree = do
  addType
    "type Tree a = Branch (Tree a) a (Tree a) | Leaf a"
  addBinding
    "invertTree"
    ( mconcat
        [ "let invert tree = ",
          "match tree with ",
          "(Branch left a right) -> Branch (invert right) a (invert left) ",
          "| (Leaf a) -> Leaf a; ",
          "invert"
        ]
    )

unsafeGetExpr :: Text -> StoreExpression Annotation
unsafeGetExpr input =
  case parseExpr input of
    Right expr' -> StoreExpression expr' mempty mempty
    a -> error $ "Error evaluating " <> T.unpack input <> ": " <> show a

addExprBinding ::
  Expr Name Annotation ->
  Name ->
  Project Annotation ->
  Either (Error Annotation) (Project Annotation)
addExprBinding expr name project = do
  (_, _, resolvedExpr) <-
    Actions.run
      project
      ( Actions.typecheckExpression project (prettyPrint expr) expr
      )
  let seUnit = reStoreExpression resolvedExpr $> ()
  let hash = getStoreExpressionHash seUnit
  let newEnv = fromItem name (reStoreExpression resolvedExpr) hash
  pure (project <> newEnv)
