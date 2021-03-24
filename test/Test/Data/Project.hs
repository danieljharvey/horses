{-# LANGUAGE OverloadedStrings #-}

module Test.Data.Project
  ( stdLib,
    idExpr,
    addBinding,
  )
where

import Data.Coerce
import Data.Functor
import Data.Text (Text)
import qualified Data.Text as T
import Language.Mimsa.Actions
import Language.Mimsa.Parser (parseExpr)
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
      "type Option a = Some a | Nowt in {}"
      "typeState"
    >>= addBinding
      "\\f -> \\opt -> case opt of Some \\a -> Some f(a) | otherwise Nowt"
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
    >>= addPair
    >>= addStateMonad

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
      "\\pair -> case pair of Pair (\\a -> \\b -> a)"
      "fstPair"
    >>= addBinding
      "\\pair -> case pair of Pair (\\a -> \\b -> b)"
      "sndPair"

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
      "\\f -> \\state -> case state of State (\\sas -> State (\\s -> let as = sas(s); case as of Pair (\\a -> \\s -> Pair f(a) s)))"
      "fmapState"
    >>= addBinding
      "\\stateF -> \\stateA -> State (\\s -> case stateF of State (\\sfs -> let fs = sfs(s); case fs of  Pair (\\f -> \\ss -> case stateA of State (\\sas -> let as = sas(ss); case as of Pair (\\a -> \\sss -> Pair f(a) sss)))))"
      "apState"
    >>= addBinding
      "\\f -> \\state -> State (\\s -> case state of State (\\sas -> let as = sas(s); case as of Pair (\\a -> \\ss -> case f(a) of State (\\sbs -> sbs(ss)))))"
      "bindState"
    >>= addBinding
      "\\state -> \\s -> case state of State (\\sas -> sas(s))"
      "runState"
    >>= addBinding
      "\\state -> compose(sndPair)(runState(state))"
      "execState"
    >>= addBinding
      "\\state -> compose(fstPair)(runState(state))"
      "evalState"
    >>= addBinding
      "\\s -> State (\\ignore -> Pair Unit s)"
      "putState"
    >>= addBinding
      "State (\\s -> Pair s s)"
      "getState"
    >>= addBinding
      "\\f -> State (\\s -> Pair Unit f(s))"
      "modifyState"
    >>= addBinding
      "\\f -> \\stateA -> \\stateB -> apState(fmapState(f)(stateA))(stateB)"
      "liftA2State"
    >>= addBinding
      "\\newName -> let sas = \\s -> let return = newName <> \"!!!\"; let list = cons(newName)(s); Pair return list; State sas"
      "storeName"

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
