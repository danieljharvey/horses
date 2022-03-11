{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Project.Stdlib
  ( buildStdlib,
    stdlib,
    addType,
    addBinding,
    removeBinding,
  )
where

import Data.Functor
import Data.Text (Text)
import qualified Language.Mimsa.Actions.BindExpression as Actions
import qualified Language.Mimsa.Actions.BindType as Actions
import qualified Language.Mimsa.Actions.Monad as Actions
import qualified Language.Mimsa.Actions.RemoveBinding as Actions
import Language.Mimsa.Parser
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Project

buildStdlib :: Either (Error Annotation) (Project Annotation)
buildStdlib =
  Actions.run mempty action >>= \(proj, _, _) -> pure proj
  where
    action = do
      baseFns
      addType "type Void"
      addType "type Maybe a = Just a | Nothing"
      addType "type Either e a = Left e | Right a"
      addType "type Task r a = Task ((a -> r) -> r)"
      addType "type Unit = Unit"
      addType "type Monoid a = Monoid (a -> a -> a) a"
      parserFns
      arrayFns
      monoidFns
      stringFns
      stateFns
      readerFns
      mapFns

baseFns :: Actions.ActionM ()
baseFns = do
  addBinding "id" "\\a -> a"
  addBinding "compose" "\\f -> \\g -> \\a -> f (g a)"
  addBinding "not" "\\a -> if a then False else True"
  addBinding "and" "\\a -> \\b -> if a then b else False"
  addBinding "or" "\\a -> \\b -> if a then True else b"
  addBinding "fst" "\\pair -> let (a,_) = pair in a"
  addBinding "snd" "\\pair -> let (_,b) = pair in b"

monoidFns :: Actions.ActionM ()
monoidFns = do
  addBinding
    "monoidMaybe"
    ( mconcat
        [ "\\innerM -> ",
          "Monoid (\\a -> \\b -> match (a,b) with ",
          "          (Just iA, Just iB) -> let (Monoid innerMappend innerMempty) = innerM; Just (innerMappend iA iB)",
          "        | (Just iA, Nothing) -> (Just iA) ",
          "        | (Nothing, Just iB) -> (Just iB) ",
          "        | _ -> Nothing) Nothing"
        ]
    )
  addBinding
    "monoidConcat"
    "\\monoid -> let (Monoid mappend mempty) = monoid; array.reduce (\\b -> \\a -> mappend a b) mempty"
  addBinding
    "monoidFoldMap"
    "\\monoid -> \\f -> let (Monoid mappend mempty) = monoid; (array.reduce (\\a -> \\total -> mappend total (f a)) mempty)"
  addBinding
    "monoidAny"
    "Monoid or False"
  addBinding
    "monoidAll"
    "Monoid and True"
  addBinding
    "monoidFirst"
    "Monoid (\\a -> \\b -> match ((a, b)) with (_, Just b) -> (Just b) | (a, _) -> a) (Nothing)"
  addBinding
    "monoidLast"
    "Monoid (\\a -> \\b -> match ((a, b)) with (_, Just b) -> (Just b) | (a, _) -> a) (Nothing)"
  addBinding "monoid" "{ concat: monoidConcat, foldMap: monoidFoldMap, maybe: monoidMaybe, any: monoidAny, all: monoidAll, first: monoidFirst, last: monoidLast }"
  removeBinding "monoidConcat"
  removeBinding "monoidFoldMap"
  removeBinding "monoidMaybe"
  removeBinding "monoidAll"
  removeBinding "monoidAny"
  removeBinding "monoidFirst"
  removeBinding "monoidLast"

parserFns :: Actions.ActionM ()
parserFns = do
  addType "type Parser a  = Parser (String -> Maybe (a, String))"
  addBinding "runParser" "\\parser -> \\str -> match parser with (Parser p) -> match p str with  (Just (\"\", a)) -> (Just a) | _ -> (Nothing)"
  addBinding "fmapParser" "\\f -> \\parser -> Parser (\\str -> match parser with (Parser p) -> (let outcome = p str; match outcome with (Just (a, rest)) -> (Just ((f a,rest))) | _ -> (Nothing)))"
  addBinding "bindParser" "\\f -> \\parser -> Parser (\\str -> match parser with (Parser p) -> match p str with (Just (a, rest)) -> (let nextParser = f a; match nextParser with (Parser b) -> b rest) | _ -> (Nothing))"
  addBinding "charParser" "Parser (\\s -> match s with ch ++ rest -> (Just ((rest, ch))) | _ -> (Nothing))"
  addBinding "predParser" "\\pred -> \\p -> Parser (\\s -> let (Parser inner) = p; match inner s with (Just (rest, a)) -> (if pred a then (Just ((rest, a))) else (Nothing)) | _ -> (Nothing))"
  addBinding "parser" "{ run: runParser, fmap: fmapParser, bind: bindParser, char: charParser, pred: predParser }"
  removeBinding "runParser"
  removeBinding "fmapParser"
  removeBinding "bindParser"
  removeBinding "charParser"
  removeBinding "predParser"

stateFns :: Actions.ActionM ()
stateFns = do
  addType "type State s a = State (s -> (a, s))"
  addBinding "pureState" "\\a -> State (\\s -> (a, s))"
  addBinding "fmapState" "\\f -> \\state -> match state with (State sas) -> State (\\s -> let as = sas s; match as with (a, s) -> (f a, s))"
  addBinding "apState" "\\stateF -> \\stateA -> State (\\s -> match stateF with (State sfs) -> let fs = sfs s; match fs with (f, ss) -> match stateA with (State sas) -> let as = sas ss; match as with (a, sss) -> (f a, sss))"
  addBinding "bindState" "\\f -> \\state -> State (\\s -> match state with (State sas) -> match (sas s) with (a, ss) -> match f a with (State sbs) -> sbs ss)"
  addBinding "runState" "\\state -> \\s -> match state with (State sas) -> sas s"
  addBinding "execState" "\\state -> compose snd (runState state)"
  addBinding "evalState" "\\state -> compose fst (runState state)"
  addBinding "liftA2State" "\\f -> \\stateA -> \\stateB -> apState (fmapState f stateA) stateB"
  addBinding "state" "{ pure: pureState, fmap: fmapState, ap: apState, bind: bindState, run: runState, exec: execState, eval: evalState, liftA2: liftA2State }"
  removeBinding "pureState"
  removeBinding "fmapState"
  removeBinding "apState"
  removeBinding "bindState"
  removeBinding "runState"
  removeBinding "execState"
  removeBinding "evalState"
  removeBinding "liftA2State"

arrayFns :: Actions.ActionM ()
arrayFns = do
  addBinding "arrayReduce" "let arrayReduce = \\f -> \\def -> \\as -> match as with [] -> def | [a, ...rest] -> arrayReduce f (f a def) rest; arrayReduce"
  addBinding "arrayReverse" "arrayReduce (\\all -> \\a -> [ all ] <> a) []"
  addBinding "arrayMap" "\\f -> arrayReduce (\\a -> \\all -> all <> [ f a ]) []"
  addBinding "arrayFilter" "\\pred -> arrayReduce (\\a -> \\all -> if pred a then all <> [ a ] else all) []"
  addBinding "arrayMonoid" "Monoid (\\a -> \\b -> a <> b) []"
  addBinding "arrayFind" "\\pred -> arrayReduce (\\item -> \\total -> match total with (Just found) -> (Just found) | _ -> (if (pred item) then (Just item) else Nothing)) Nothing"
  addBinding "array" "{ reduce: arrayReduce, reverse: arrayReverse, map: arrayMap, filter: arrayFilter, find: arrayFind, monoid: arrayMonoid }"
  removeBinding "arrayReduce"
  removeBinding "arrayReverse"
  removeBinding "arrayMap"
  removeBinding "arrayFilter"
  removeBinding "arrayMonoid"
  removeBinding "arrayFind"

stringFns :: Actions.ActionM ()
stringFns = do
  addBinding "stringReduce" "let stringReduce = \\f -> \\def -> \\str -> match str with \"\" -> def | head ++ tail -> stringReduce f (f def head) tail; stringReduce"
  addBinding "stringMap" "\\f -> stringReduce (\\total -> \\a -> total ++ f a) \"\""
  addBinding "stringFilter" "\\pred -> stringReduce (\\all -> \\a -> if pred a then all ++ a else all) \"\""
  addBinding "stringSplit" "\\char -> \\str -> array.reverse (stringReduce (\\as -> \\a -> if (a == char) then [ \"\" ] <> as else match as with [] -> [] | [current, ...rest] -> [ current ++ a ] <> rest) [\"\"] str)"
  addBinding "stringMonoid" "Monoid (\\a -> \\b -> a ++ b) \"\""
  addBinding "string" "{ reduce: stringReduce, map: stringMap, filter: stringFilter, split: stringSplit, monoid: stringMonoid }"
  removeBinding "stringReduce"
  removeBinding "stringMap"
  removeBinding "stringFilter"
  removeBinding "stringSplit"
  removeBinding "stringMonoid"

readerFns :: Actions.ActionM ()
readerFns = do
  addType "type Reader r a = Reader (r -> a)"
  addBinding "readerRun" "\\reader -> \\r -> let (Reader ra) = reader in (ra r)"
  addBinding "readerAsk" "Reader (\\r -> r)"
  addBinding "readerLocal" "\\envF -> \\reader -> Reader (\\r -> readerRun reader (envF r))"
  addBinding "readerAp" "\\readerF -> \\readerA -> let (Reader rToF) = readerF; let (Reader rToA) = readerA; (Reader (\\r -> rToF r (rToA r)))"
  addBinding "readerMonoid" "\\innerM -> let (Monoid append empty) = innerM; (Monoid (\\rA -> \\rB -> Reader (\\r -> append (readerRun rA r) (readerRun rB r))) (Reader (\\r -> empty)))"
  addBinding "reader" "{ run: readerRun, ask: readerAsk, local: readerLocal, fmap: reader.fmap, pure: reader.pure, ap: readerAp, monoid: readerMonoid }"
  removeBinding "readerRun"
  removeBinding "readerAsk"
  removeBinding "readerLocal"
  removeBinding "readerAp"
  removeBinding "readerMonoid"

mapFns :: Actions.ActionM ()
mapFns = do
  addType "type Map k a = Map [(k,a)]"
  addBinding "mapEmpty" "Map []"
  addBinding "mapDelete" "let delete k map = let (Map inner) = map; (Map (array.filter (\\val -> let (key, _) = val; (not (key == k))) inner)); delete"
  addBinding "mapInsert" "let insert k v map = let (Map inner) = (mapDelete k map); (Map ([ ((k, v)) ] <> inner)); insert"
  addBinding "mapLookup" "let lookup k map = let (Map inner) = map; (array.find (\\item -> let (key, value) = item; k == key) inner); lookup"
  addBinding "mapInsert" "let insert k v map = let (Map inner) = (mapDelete k map); (Map ([ ((k, v)) ] <> inner)); insert"
  addBinding "map" "{ empty: mapEmpty, insert: mapInsert, lookup: mapLookup, insert: mapInsert }"
  removeBinding "mapEmpty"
  removeBinding "mapInsert"
  removeBinding "mapDelete"
  removeBinding "mapLookup"
  removeBinding "mapInsert"

addType :: Text -> Actions.ActionM ()
addType t =
  let dt = fromRight $ parseAndFormat typeDeclParser t
   in Actions.bindType t dt $> ()

addBinding :: Name -> Text -> Actions.ActionM ()
addBinding name b =
  let expr =
        fromRight $ parseAndFormat expressionParser b
   in Actions.bindExpression
        expr
        name
        b
        $> ()

removeBinding :: Name -> Actions.ActionM ()
removeBinding = Actions.removeBinding

fromRight :: (Show e) => Either e a -> a
fromRight = \case
  Left e -> error (show e)
  Right a -> a

stdlib :: Project Annotation
stdlib = fromRight buildStdlib
