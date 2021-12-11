{-# LANGUAGE OverloadedStrings #-}

module Test.Data.Project
  ( testStdlib,
    idExpr,
    addBinding,
    addExprBinding,
  )
where

import Data.Coerce
import Data.Functor
import Data.Text (Text)
import qualified Data.Text as T
import qualified Language.Mimsa.Actions.Monad as Actions
import qualified Language.Mimsa.Actions.Shared as Actions
import Language.Mimsa.Parser (parseExpr)
import Language.Mimsa.Printer
import Language.Mimsa.Project.Stdlib
import Language.Mimsa.Store.Hashing
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
  Actions.run mempty action >>= \(proj, _, _) -> pure proj
  where
    action = do
      addBinding
        "id"
        "\\a -> a"
      addBinding
        "const"
        "\\a -> \\b -> a"
      addBinding
        "compose"
        "\\f -> \\g -> \\a -> f g a"
      addBinding
        "fst"
        "\\tuple -> let (tupleFirst,tupleSecond) = tuple in tupleFirst"
      addBinding
        "snd"
        "\\tuple -> let (tupleFirst,tupleSecond) = tuple in tupleSecond"
      addBinding
        "and"
        "\\a -> \\b -> if a then True else b"
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
        "compose"
        "\\f -> \\g -> \\a -> f (g a)"
      addBinding
        "incrementInt"
        "\\a -> addInt 1 a"
      addBinding
        "typeState"
        "type Maybe a = Just a | Nothing in {}"
      addBinding
        "fmapMaybe"
        "\\f -> \\opt -> match opt with (Just a) -> Just (f a) | _ -> Nothing"
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
      addEither
      addPair
      addStateMonad
      addParser
      addArray
      addIdentity
      addMonoid
      addMonoPair
      addTree

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

addPair :: Actions.ActionM ()
addPair = do
  addType
    "type Pair a b = Pair a b"
  addBinding
    "fstPair"
    "\\pair -> match pair with (Pair a _) -> a"
  addBinding
    "sndPair"
    "\\pair -> match pair with (Pair _ b) -> b"

addEither :: Actions.ActionM ()
addEither =
  addType
    "type Either e a = Left e | Right a"

addStateMonad :: Actions.ActionM ()
addStateMonad = do
  addType
    "type State s a = State (s -> (Pair a s))"
  addBinding
    "pureState"
    "\\a -> State (\\s -> Pair a s)"
  addBinding
    "fmapState"
    "\\f -> \\state -> match state with (State sas) -> State (\\s -> let as = sas s; match as with (Pair a s) -> Pair (f a) s)"
  addBinding
    "apState"
    "\\stateF -> \\stateA -> State (\\s -> match stateF with (State sfs) -> let fs = sfs s; match fs with (Pair f ss) -> match stateA with (State sas) -> let as = sas ss; match as with (Pair a sss) -> Pair (f a) sss)"
  addBinding
    "bindState"
    "\\f -> \\state -> State (\\s -> match state with (State sas) -> let as = sas s; match as with (Pair a ss) -> match f a with (State sbs) -> sbs ss)"
  addBinding
    "runState"
    "\\state -> \\input -> match state with (State sas) -> sas input"
  addBinding
    "execState"
    "\\state -> compose sndPair (runState state)"
  addBinding
    "evalState"
    "\\state -> compose fstPair (runState state)"
  addBinding
    "liftA2State"
    "\\f -> \\stateA -> \\stateB -> apState (fmapState f stateA) stateB"
  addBinding
    "storeName"
    "\\newName -> let sas = (\\s -> let return = newName ++ \"!!!\"; let list = cons newName s; Pair return list) in State sas"
  addBinding
    "testStateUsages"
    "(evalState, execState)"

addParser :: Actions.ActionM ()
addParser = do
  addType
    "type Parser a = Parser (String -> Maybe (String,a))"
  addBinding
    "anyChar"
    "let p = (\\str -> match str with (c ++ rest) -> (Just (rest, c)) | _ -> Nothing) in Parser p"
  addBinding
    "runParser"
    "\\p -> \\str -> match p with (Parser parser) -> match parser str with (Just (rest, a)) -> Just a | _ -> Nothing"
  addBinding
    "fmapParser"
    "\\f -> \\p -> match p with (Parser parser) -> Parser (\\s -> match parser s with (Just (rest, a)) -> Just (rest, f a) | _ -> Nothing)"
  addBinding
    "bindParser"
    "\\f -> \\p -> match p with (Parser parser) -> Parser (\\s -> match parser s with (Just (restA, a)) -> (let nextParser = match f a with (Parser parserB) -> parserB; nextParser restA) | _ -> Nothing)"
  addBinding
    "predParser"
    "\\pred -> \\p -> Parser (\\s -> let (Parser psr) = p in match psr s with (Just (rest, a)) -> (if pred a then (Just ((rest, a))) else (Nothing)) | _ -> (Nothing))"
  addBinding
    "failParser"
    "Parser \\s -> Nothing"

addArray :: Actions.ActionM ()
addArray =
  addBinding
    "mapArray"
    "\\f -> \\arr -> let map = \\as -> match as with [a, ...rest] -> [f a] <> map rest | _ -> []; map arr"

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
addExprBinding expr name env = do
  resolvedExpr <-
    Actions.getTypecheckedStoreExpression (prettyPrint expr) env expr
  let seUnit = reStoreExpression resolvedExpr $> ()
  let hash = coerce $ snd $ contentAndHash (storeExpression seUnit)
  let newEnv = Actions.fromItem name (reStoreExpression resolvedExpr) hash
  pure (env <> newEnv)
