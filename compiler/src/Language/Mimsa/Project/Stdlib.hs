{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Project.Stdlib
  ( buildStdlib,
    stdlib,
    addType,
    addBinding,
    removeBinding,
    baseFns,
    arrayFns,
    nonEmptyArrayFns,
    allFns,
  )
where

import Data.Functor
import Data.Text (Text)
import qualified Language.Mimsa.Actions.AddUnitTest as Actions
import qualified Language.Mimsa.Actions.BindExpression as Actions
import qualified Language.Mimsa.Actions.BindType as Actions
import qualified Language.Mimsa.Actions.Monad as Actions
import qualified Language.Mimsa.Actions.RemoveBinding as Actions
import Language.Mimsa.Parser
import Language.Mimsa.Tests.Types
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Project

buildStdlib :: Either (Error Annotation) (Project Annotation)
buildStdlib =
  Actions.run mempty allFns >>= \(proj, _, _) -> pure proj

allFns :: Actions.ActionM ()
allFns = do
  baseFns
  arrayFns
  nonEmptyArrayFns
  parserFns
  monoidFns
  stringFns
  stateFns
  readerFns
  mapFns
  jsonFns
  personTestFns

baseFns :: Actions.ActionM ()
baseFns = do
  addBinding "id" "\\a -> a"
  addTest "id does nothing" "\\a -> id a == a"
  addBinding "compose" "\\f -> \\g -> \\a -> f (g a)"
  addBinding "not" "\\a -> if a then False else True"
  addTest "Running not twice is identity on booleans" "\\a -> not (not a) == a"
  addBinding "and" "\\a -> \\b -> if a then b else False"
  addBinding "or" "\\a -> \\b -> if a then True else b"
  addBinding "fst" "\\pair -> let (a,_) = pair in a"
  addBinding "snd" "\\pair -> let (_,b) = pair in b"
  addBinding "const" "\\a -> \\b -> a"
  addType "type Void"
  addType "type Maybe a = Just a | Nothing"
  addType "type Either e a = Left e | Right a"
  addType "type Unit = Unit"
  addType "type Monoid a = Monoid (a -> a -> a) a"

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
  addBinding "runParser" "\\parser -> \\str -> match parser with (Parser p) -> match p str with  (Just (a, _)) -> (Just a) | _ -> (Nothing)"
  addBinding "fmapParser" "\\f -> \\parser -> Parser (\\str -> match parser with (Parser p) -> (match p str with (Just (a, rest)) -> (Just ((f a,rest))) | _ -> (Nothing)))"
  addBinding "apParser" "\\parserF -> \\parserA -> let (Parser pF) = parserF; let (Parser pA) = parserA; Parser (\\input -> match (pF input) with Just (f, input2) -> (match (pA input2) with Just (a, input3) -> Just (f a, input3) | _ -> Nothing) | _ ->  Nothing)"
  addBinding "bindParser" "\\f -> \\parser -> Parser (\\input -> let (Parser firstP) = parser; match (firstP input) with (Just (a, input2)) -> let (Parser secondP) = (f a); (secondP input2) | _ -> Nothing)"
  addBinding "anyCharParser" "Parser (\\s -> match s with ch ++ rest -> (Just ((ch, rest))) | _ -> (Nothing))"
  addBinding "predParser" "\\pred -> \\p -> Parser (\\s -> let (Parser inner) = p; match inner s with (Just (a, rest)) -> (if pred a then (Just ((a, rest))) else (Nothing)) | _ -> (Nothing))"
  addBinding "altParser" "let runParse p input = let (Parser pp) = p in (pp input); \\p1 -> \\p2 -> Parser (\\input -> match (runParse p1 input) with (Just a) -> (Just a) | (Nothing) -> (runParse p2 input))"
  addBinding "charParser" "\\char -> predParser (\\c -> c == char) anyCharParser"
  addBinding "manyParser" "\\parser -> let (Parser innerP) = parser; (Parser (\\input -> let go items i = match (innerP i) with (Just (a, i2)) -> (go (items <> [ a ]) i2) | (Nothing) -> (Just ((items, i))); go [] input))"
  addBinding "liftA2Parser" "\\f -> \\p1 -> apParser (fmapParser f p1)"
  addBinding "pairParser" "liftA2Parser (\\a -> \\b -> (a,b))"
  addBinding "leftParser" "\\p1 -> \\p2 -> fmapParser fst (pairParser p1 p2)"
  addBinding "rightParser" "\\p1 -> \\p2 -> fmapParser snd (pairParser p1 p2)"
  addBinding "someParser" "\\p -> liftA2Parser NonEmptyArray p (manyParser p)"
  addBinding "whitespaceParser" "infix <|> = altParser; charParser \" \" <|> charParser \"\n\" <|> charParser \"\r\""
  addBinding "space0Parser" "fmapParser (const Unit) (manyParser whitespaceParser)"
  addBinding "space1Parser" "fmapParser (const Unit) (someParser whitespaceParser)"
  addBinding "sepByParser" "\\sepP -> \\p -> let pairP = rightParser sepP p; liftA2Parser NonEmptyArray p (manyParser pairP)"
  addBinding "parser" "{ run: runParser, fmap: fmapParser, bind: bindParser, anyChar: anyCharParser, char: charParser, pred: predParser, alt: altParser, many: manyParser, ap: apParser, liftA2: liftA2Parser, pair: pairParser, left: leftParser, right: rightParser, some: someParser, whitespace: whitespaceParser, space0: space0Parser, space1: space1Parser, sepBy: sepByParser }"
  removeBinding "apParser"
  removeBinding "runParser"
  removeBinding "fmapParser"
  removeBinding "bindParser"
  removeBinding "charParser"
  removeBinding "predParser"
  removeBinding "altParser"
  removeBinding "anyCharParser"
  removeBinding "manyParser"
  removeBinding "liftA2Parser"
  removeBinding "pairParser"
  removeBinding "leftParser"
  removeBinding "rightParser"
  removeBinding "someParser"
  removeBinding "whitespaceParser"
  removeBinding "space0Parser"
  removeBinding "space1Parser"
  removeBinding "sepByParser"
  addTest "parser.char parses a specific char" "parser.run (parser.char \"a\") \"a\" == Just \"a\""

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
  addBinding "mapLookup" "let lookup k map = let (Map inner) = map; (maybe.fmap snd (array.find (\\item -> let (key, value) = item; k == key) inner)); lookup"
  addBinding "map" "{ empty: mapEmpty, delete: mapDelete, lookup: mapLookup, insert: mapInsert }"
  removeBinding "mapEmpty"
  removeBinding "mapDelete"
  removeBinding "mapInsert"
  removeBinding "mapLookup"

nonEmptyArrayFns :: Actions.ActionM ()
nonEmptyArrayFns = do
  addType "type NonEmptyArray a = NonEmptyArray a [a]"
  addBinding "neFmap" "\\f -> \\ne -> let (NonEmptyArray a as) = ne; NonEmptyArray (f a) (array.map f as)"
  addBinding "nonEmptyArray" "{ fmap: neFmap }"
  removeBinding "neFmap"

jsonFns :: Actions.ActionM ()
jsonFns = do
  addType "type Json = JString String | JNumber Int | JNull | JArray [Json] | JRecord (Map String Json) | JBoolean Boolean"
  addBinding "jsonTypeName" "\\json -> match json with (JRecord _) -> \"record\" | (JArray _) -> \"array\" | (JString _) -> \"string\" | (JNumber _) -> \"number\" | (JNull) -> \"null\" | (JBoolean _) -> \"boolean\""
  addBinding "jsonRecord" "\\json -> match json with (JRecord record) -> (Right record) | other -> Left (\"Expected record, got \" ++ jsonTypeName other)"
  addBinding "jsonString" "\\json -> match json with (JString s) -> Right s | other -> Left (\"Expected string, got \" ++ jsonTypeName other)"
  addBinding "jsonNull" "\\json -> match json with JNull -> Right Unit | other -> Left (\"Expected null, got \" ++ jsonTypeName other)"
  addBinding "jsonNumber" "\\json -> match json with JNumber i -> Right i | other -> Left (\"Expected number, got \" ++ jsonTypeName other)"
  addBinding "jsonArray" "\\json -> match json with JArray as -> Right as | other -> Left (\"Expected array, got \" ++ jsonTypeName other)"
  addBinding "jsonBoolean" "\\json -> match json with JBoolean b -> Right b | other -> Left (\"Expected boolean, got \" ++ jsonTypeName other)"
  addBinding "jsonLookupRecord" "\\label -> \\json -> match (jsonRecord json) with (Right inner) -> (match (map.lookup label inner) with (Just a) -> (Right a) | _ -> (Left (\"Could not find an entry for \" ++ label))) | (Left e) -> (Left e)"
  addBinding "json" "{ getRecord: jsonRecord, getString: jsonString, getNull: jsonNull, getNumber: jsonNumber, getArray: jsonArray, getBoolean: jsonBoolean, lookupRecord: jsonLookupRecord }"
  removeBinding "jsonTypeName"
  removeBinding "jsonRecord"
  removeBinding "jsonString"
  removeBinding "jsonNull"
  removeBinding "jsonNumber"
  removeBinding "jsonArray"
  removeBinding "jsonBoolean"
  removeBinding "jsonLookupRecord"

personTestFns :: Actions.ActionM ()
personTestFns = do
  addType "type Person = Person { name: String, age: Int }"
  addBinding "personToJson" "\\person -> let (Person p) = person in JRecord (Map [(\"name\",JString p.name),(\"age\", JNumber p.age)])"
  addBinding "personFromJson" "let bindEither f either = match either with Right a -> f a | Left e -> Left e; \\input -> let eName = bindEither json.getString (json.lookupRecord \"name\" input); let eAge = bindEither json.getNumber (json.lookupRecord \"age\" input); let f name age = Person { name, age}; either.ap (either.fmap f eName) eAge"
  addTest "Round trip JSON encoding test for Person" "\\person -> match personFromJson (personToJson person) with Right per -> per == person | _ -> False"

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

addTest :: Text -> Text -> Actions.ActionM ()
addTest label input = do
  let expr = fromRight $ parseAndFormat expressionParser input
  _ <- Actions.addUnitTest expr (TestName label) input
  pure ()

removeBinding :: Name -> Actions.ActionM ()
removeBinding = Actions.removeBinding

fromRight :: (Show e) => Either e a -> a
fromRight = \case
  Left e -> error (show e)
  Right a -> a

stdlib :: Project Annotation
stdlib = fromRight buildStdlib
