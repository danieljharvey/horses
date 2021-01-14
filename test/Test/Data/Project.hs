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
      (mkName "id")
    >>= addBinding
      "\\tuple -> let (tupleFirst,tupleSecond) = tuple in tupleFirst"
      (mkName "fst")
    >>= addBinding
      "\\tuple -> let (tupleFirst,tupleSecond) = tuple in tupleSecond"
      (mkName "snd")
    >>= addBinding
      "\\a -> \\b -> a == b"
      (mkName "eq")
    >>= addBinding
      "\\i -> eq(10)(i)"
      (mkName "eqTen")
    >>= addBinding
      "\\a -> \\b -> a + b"
      (mkName "addInt")
    >>= addBinding
      "\\f -> \\g -> \\aValue -> f(g(aValue))"
      (mkName "compose")
    >>= addBinding
      "\\a -> addInt(1)(a)"
      (mkName "incrementInt")
    >>= addBinding
      "type Option a = Some a | Nowt in {}"
      (mkName "typeState")
    >>= addBinding
      "\\f -> \\opt -> case opt of Some \\a -> Some f(a) | otherwise Nowt"
      (mkName "fmapOption")
    >>= addBinding
      "type These a b = This a | That b | These a b in {}"
      (mkName "typeThese")
    >>= addBinding
      "(1,2)"
      (mkName "aPair")
    >>= addBinding
      "{ a: 1, b: \"dog\" }"
      (mkName "aRecord")
    >>= addListMonad
    >>= addPair
    >>= addStateMonad

addListMonad :: Project Annotation -> ProjectPart
addListMonad prj =
  pure prj
    >>= addBinding
      "type List a = Cons a (List a) | Nil in {}"
      (mkName "typeList")
    >>= addBinding
      "\\a -> \\list -> Cons a list"
      (mkName "cons")
    >>= addBinding "Nil" (mkName "nil")

addPair :: Project Annotation -> ProjectPart
addPair prj =
  pure prj
    >>= addBinding
      "type Pair a b = Pair a b in {}"
      (mkName "typePair")
    >>= addBinding
      "\\pair -> case pair of Pair (\\a -> \\b -> a)"
      (mkName "fstPair")
    >>= addBinding
      "\\pair -> case pair of Pair (\\a -> \\b -> b)"
      (mkName "sndPair")

addStateMonad :: Project Annotation -> ProjectPart
addStateMonad prj =
  pure prj
    >>= addBinding
      "type State s a = State (s -> (Pair a s)) in {}"
      (mkName "typeState")
    >>= addBinding
      "\\a -> State (\\s -> Pair a s)"
      (mkName "pureState")
    >>= addBinding
      "\\f -> \\state -> case state of State (\\sas -> State (\\s -> let as = sas(s); case as of Pair (\\a -> \\s -> Pair f(a) s)))"
      (mkName "fmapState")
    >>= addBinding
      "\\stateF -> \\stateA -> State (\\s -> case stateF of State (\\sfs -> let fs = sfs(s); case fs of  Pair (\\f -> \\ss -> case stateA of State (\\sas -> let as = sas(ss); case as of Pair (\\a -> \\sss -> Pair f(a) sss)))))"
      (mkName "apState")
    >>= addBinding
      "\\f -> \\state -> State (\\s -> case state of State (\\sas -> let as = sas(s); case as of Pair (\\a -> \\ss -> case f(a) of State (\\sbs -> sbs(ss)))))"
      (mkName "bindState")
    >>= addBinding
      "\\state -> \\s -> case state of State (\\sas -> sas(s))"
      (mkName "runState")
    >>= addBinding
      "\\state -> compose(sndPair)(runState(state))"
      (mkName "execState")
    >>= addBinding
      "\\state -> compose(fstPair)(runState(state))"
      (mkName "evalState")
    >>= addBinding
      "\\s -> State (\\ignore -> Pair Unit s)"
      (mkName "putState")
    >>= addBinding
      "State (\\s -> Pair s s)"
      (mkName "getState")
    >>= addBinding
      "\\f -> State (\\s -> Pair Unit f(s))"
      (mkName "modifyState")
    >>= addBinding
      "\\f -> \\stateA -> \\stateB -> apState(fmapState(f)(stateA))(stateB)"
      (mkName "liftA2State")
    >>= addBinding
      "\\newName -> let sas = \\s -> let return = newName <> \"!!!\"; let list = cons(newName)(s); Pair return list; State sas"
      (mkName "storeName")

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
