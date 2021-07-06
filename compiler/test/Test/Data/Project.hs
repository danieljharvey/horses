{-# LANGUAGE OverloadedStrings #-}

module Test.Data.Project
  ( stdLib,
    idExpr,
    addBinding,
    addExprBinding,
  )
where

import Data.Coerce
import Data.Functor
import Data.Text (Text)
import qualified Data.Text as T
import Language.Mimsa.Actions
import Language.Mimsa.Parser (parseExpr)
import Language.Mimsa.Printer
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
stdLib :: Project Annotation
stdLib = case stdLibE of
  Right stdLib' -> (stdLib' $> ()) $> mempty
  Left e -> error (show e)

type ProjectPart = Either (Error Annotation) (Project Annotation)

stdLibE :: ProjectPart
stdLibE =
  pure mempty
    >>= addBinding
      "\\a -> a"
      "id"
    >>= addBinding
      "\\f -> \\g -> \\a -> f(g(a))"
      "compose"
    >>= addBinding
      "\\tuple -> let (tupleFirst,tupleSecond) = tuple in tupleFirst"
      "fst"
    >>= addBinding
      "\\tuple -> let (tupleFirst,tupleSecond) = tuple in tupleSecond"
      "snd"
    >>= addBinding
      "\\a -> \\b -> a == b"
      "eq"
    >>= addBinding
      "\\i -> eq(10)(i)"
      "eqTen"
    >>= addBinding
      "\\a -> \\b -> a + b"
      "addInt"
    >>= addBinding
      "\\a -> \\b -> a - b"
      "subtractInt"
    >>= addBinding
      "\\f -> \\g -> \\aValue -> f(g(aValue))"
      "compose"
    >>= addBinding
      "\\a -> addInt(1)(a)"
      "incrementInt"
    >>= addBinding
      "type Option a = Some a | None in {}"
      "typeState"
    >>= addBinding
      "\\f -> \\opt -> match opt with (Some a) -> Some f(a) | _ -> None"
      "fmapOption"
    >>= addBinding
      "type These a b = This a | That b | These a b in {}"
      "typeThese"
    >>= addBinding
      "(1,2)"
      "aPair"
    >>= addBinding
      "{ a: 1, b: \"dog\" }"
      "aRecord"
    >>= addBinding
      "type Person = Person { name: String, age: Int } in {}"
      "typePerson"
    >>= addListMonad
    >>= addEither
    >>= addPair
    >>= addStateMonad
    >>= addParser
    >>= addArray
    >>= addIdentity

addListMonad :: Project Annotation -> ProjectPart
addListMonad prj =
  pure prj
    >>= addBinding
      "type List a = Cons a (List a) | Nil in {}"
      "typeList"
    >>= addBinding
      "\\a -> \\list -> Cons a list"
      "cons"
    >>= addBinding "Nil" "nil"

addPair :: Project Annotation -> ProjectPart
addPair prj =
  pure prj
    >>= addBinding
      "type Pair a b = Pair a b in {}"
      "typePair"
    >>= addBinding
      "\\pair -> match pair with (Pair a _) -> a"
      "fstPair"
    >>= addBinding
      "\\pair -> match pair with (Pair _ b) -> b"
      "sndPair"

addEither :: Project Annotation -> ProjectPart
addEither prj =
  pure prj
    >>= addBinding
      "type Either e a = Left e | Right a in {}"
      "typeEither"

addStateMonad :: Project Annotation -> ProjectPart
addStateMonad prj =
  pure prj
    >>= addBinding
      "type State s a = State (s -> (Pair a s)) in {}"
      "typeState"
    >>= addBinding
      "\\a -> State (\\s -> Pair a s)"
      "pureState"
    >>= addBinding
      "\\f -> \\state -> match state with (State sas) -> State (\\s -> let as = sas(s); match as with (Pair a s) -> Pair f(a) s)"
      "fmapState"
    >>= addBinding
      "\\stateF -> \\stateA -> State (\\s -> match stateF with (State sfs) -> let fs = sfs(s); match fs with (Pair f ss) -> match stateA with (State sas) -> let as = sas(ss); match as with (Pair a sss) -> Pair f(a) sss)"
      "apState"
    >>= addBinding
      "\\f -> \\state -> State (\\s -> match state with (State sas) -> let as = sas(s); match as with (Pair a ss) -> match f(a) with (State sbs) -> sbs(ss))"
      "bindState"
    >>= addBinding
      "\\state -> \\s -> match state with (State sas) -> sas(s)"
      "runState"
    >>= addBinding
      "\\state -> compose(sndPair)(runState(state))"
      "execState"
    >>= addBinding
      "\\state -> compose(fstPair)(runState(state))"
      "evalState"
    >>= addBinding
      "\\f -> \\stateA -> \\stateB -> apState(fmapState(f)(stateA))(stateB)"
      "liftA2State"
    >>= addBinding
      "\\newName -> let sas = \\s -> let return = newName ++ \"!!!\"; let list = cons(newName)(s); Pair return list; State sas"
      "storeName"

addParser :: Project Annotation -> ProjectPart
addParser prj =
  pure prj
    >>= addBinding
      "type Parser a = Parser (String -> Option (String,a)) in {}"
      "typeParser"
    >>= addBinding
      "let p = (\\str -> match str with (c ++ rest) -> (Some (rest, c)) | _ -> None) in Parser p"
      "anyChar"
    >>= addBinding
      "\\p -> \\str -> match p with (Parser parser) -> match parser(str) with (Some (rest, a)) -> Some a | _ -> None"
      "runParser"
    >>= addBinding
      "\\f -> \\p -> match p with (Parser parser) -> Parser (\\s -> match parser(s) with (Some (rest, a)) -> Some (rest, f(a)) | _ -> None)"
      "fmapParser"
    >>= addBinding
      "\\f -> \\p -> match p with (Parser parser) -> Parser (\\s -> match parser(s) with (Some (restA, a)) -> (let nextParser = match f(a) with (Parser parserB) -> parserB; nextParser(restA)) | _ -> None)"
      "bindParser"
    >>= addBinding
      "Parser \\s -> None"
      "failParser"

addArray :: Project Annotation -> ProjectPart
addArray prj =
  pure prj
    >>= addBinding
      "\\f -> \\arr -> let map = \\as -> match as with [a, ...rest] -> [f(a)] <> map(rest) | _ -> []; map(arr)"
      "mapArray"

addIdentity :: Project Annotation -> ProjectPart
addIdentity prj = pure prj >>= addBinding "type Identity a = Identity a in {}" "typeIdentity"

unsafeGetExpr :: Text -> StoreExpression Annotation
unsafeGetExpr input =
  case parseExpr input of
    Right expr' -> StoreExpression expr' mempty mempty
    a -> error $ "Error evaluating " <> T.unpack input <> ": " <> show a

addBinding ::
  Text ->
  Name ->
  Project Annotation ->
  Either (Error Annotation) (Project Annotation)
addBinding input name env = do
  (ResolvedExpression _ se _ _ _) <-
    evaluateText env input
  let seUnit = se $> ()
  let hash = coerce $ snd $ contentAndHash (storeExpression seUnit)
  let newEnv = fromItem name se hash
  pure (env <> newEnv)

addExprBinding ::
  Expr Name Annotation ->
  Name ->
  Project Annotation ->
  Either (Error Annotation) (Project Annotation)
addExprBinding expr name env = do
  (ResolvedExpression _ se _ _ _) <-
    getTypecheckedStoreExpression (prettyPrint expr) env expr
  let seUnit = se $> ()
  let hash = coerce $ snd $ contentAndHash (storeExpression seUnit)
  let newEnv = fromItem name se hash
  pure (env <> newEnv)
