{-# LANGUAGE OverloadedStrings #-}

module Test.Backend.TypescriptEndToEnd
  ( spec,
  )
where

-- these are the backend tests that invoke the whole idea of a project

import Control.Monad.Except
import Data.Bifunctor
import Data.Foldable
import Data.Functor
import Data.Hashable
import Data.Text (Text)
import qualified Data.Text as T
import Language.Mimsa.Backend.Types
import Language.Mimsa.Backend.Typescript.FromExpr
import Language.Mimsa.Backend.Typescript.Monad
import Language.Mimsa.Backend.Typescript.Printer
import Language.Mimsa.Core
import Language.Mimsa.Project.Stdlib
import Test.Backend.RunNode hiding (spec)
import Test.Hspec
import Test.Utils.Compilation
import Test.Utils.Helpers
import Test.Utils.Serialisation

testFromInputText :: Text -> Either Text Text
testFromInputText input =
  case evaluateText stdlib input of
    Left e -> throwError (prettyPrint e)
    Right typedExpr -> do
      let readerState = TSReaderState mempty mempty
          startState = TSCodegenState mempty mempty mempty
      first
        prettyPrint
        (printModule . fst <$> fromExpr readerState startState typedExpr)

-- test that we have a valid Typescript module by saving it and running it
testTypescriptInNode :: Text -> IO String
testTypescriptInNode ts = do
  -- write file
  tsPath <- createOutputFolder "Typescript"
  let tsFilename = tsPath <> show (hash ts) <> ".ts"
  -- cache output
  cachePath <- createOutputFolder "Typescript-result"
  let cacheFilename = cachePath <> show (hash ts) <> ".json"
  -- create output
  let tsOutput = ts <> "\nconsole.log(main)"
  writeFile tsFilename (T.unpack tsOutput)
  (ec, err) <- withCache cacheFilename (runTypescriptFromFile tsFilename)
  if ec then pure err else fail err

-- test that we have a valid Typescript module by saving it and running it
testTypescriptFileInNode :: FilePath -> IO String
testTypescriptFileInNode tsFilename = do
  -- create output
  (ec, err) <- runTypescriptFromFile tsFilename
  if ec then pure err else fail err

testIt :: (Text, Text, String) -> Spec
testIt (expr, expectedTS, expectedValue) =
  it (T.unpack expr) $ do
    case testFromInputText expr of
      Left e -> fail (T.unpack e)
      Right ts -> do
        ts `shouldBe` expectedTS
        val <- testTypescriptInNode ts
        val `shouldBe` expectedValue

fullTestIt :: (Text, String) -> Spec
fullTestIt (input, expectedValue) =
  it (T.unpack input) $ do
    let unsafeParse = ($> mempty) . unsafeParseExpr
        expr = unsafeParse input
    (filename, contentHash) <- testProjectCompile "CompileTSProject" Typescript expr
    cachePath <- createOutputFolder "CompileTSProject-result"
    let cacheFilename = cachePath <> show contentHash <> ".json"

    result <- withCache cacheFilename (testTypescriptFileInNode filename)
    result `shouldBe` expectedValue

testModule :: (Text, String) -> Spec
testModule (input, expectedValue) =
  it (T.unpack input) $ do
    (filename, contentHash) <- testModuleCompile "CompileTSModuleProject" Typescript input
    cachePath <- createOutputFolder "CompileTSModuleProject-result"
    let cacheFilename = cachePath <> show contentHash <> ".json"
    print filename
    result <- withCache cacheFilename (testTypescriptFileInNode filename)
    result `shouldBe` expectedValue

-- | input, output TS, nodeJS output
testCases :: [(Text, Text, String)]
testCases =
  [ ("True", "export const main = true", "true"),
    ("False", "export const main = false", "false"),
    ("123", "export const main = 123", "123"),
    ("\"Poo\"", "export const main = `Poo`", "Poo"),
    ( "if True then 1 else 2",
      "export const main = true ? 1 : 2",
      "1"
    ),
    ( "let a = \"dog\" in 123",
      "const a = `dog`; export const main = 123",
      "123"
    ),
    ( "let a = \"dog\" in let b = \"horse\" in 123",
      "const a = `dog`; \nconst b = `horse`; export const main = 123",
      "123"
    ),
    ( "{ a: 123, b: \"horse\" }",
      "export const main = { a: 123, b: `horse` }",
      "{ a: 123, b: 'horse' }"
    ),
    ("(1,2)", "export const main = [1,2] as const", "[ 1, 2 ]"),
    ("\\a -> let (b,c) = a in b", "export const main = <C,D>(a: readonly [C,D]) => { const [b,c] = a; return b; }", "[Function: main]"),
    ("2 + 2", "export const main = 2 + 2", "4"),
    ("10 - 2", "export const main = 10 - 2", "8"),
    ( "\"dog\" ++ \"log\"",
      "export const main = `dog` + `log`",
      "doglog"
    ),
    ( "{ fun: (\\a -> let d = 1 in a) }",
      "export const main = { fun: <B>(a: B) => { const d = 1; return a; } }",
      "{ fun: [Function: fun] }"
    ),
    ( "[1,2] <> [3,4]",
      "export const main = [...[1,2],...[3,4]]",
      "[ 1, 2, 3, 4 ]"
    ),
    ( "let (a, b) = (1,2) in a",
      "const [a,b] = [1,2] as const; export const main = a",
      "1"
    ),
    ( "let { dog: a, cat: b } = { dog: 1, cat: 2} in (a,b)",
      "const { cat: b, dog: a } = { cat: 2, dog: 1 }; export const main = [a,b] as const",
      "[ 1, 2 ]"
    )
  ]

-- | input, nodeJS output
fullTestCases :: [(Text, String)]
fullTestCases =
  [ ("True", "true"),
    ("False", "false"),
    ("123", "123"),
    ("\"Poo\"", "Poo"),
    ( "\\a -> a",
      "[Function: main]"
    ),
    ( "let id a = a; id 1",
      "1"
    ),
    ( "if True then 1 else 2",
      "1"
    ),
    ( "let a = \"dog\" in 123",
      "123"
    ),
    ( "let a = \"dog\" in let b = \"horse\" in 123",
      "123"
    ),
    ( "{ a: 123, b: \"horse\" }",
      "{ a: 123, b: 'horse' }"
    ),
    ( "let aPair = (1,2); let (a,b) = aPair in a",
      "1"
    ),
    ( "\\a -> let b = 123 in a",
      "[Function: main]"
    ),
    ("(1,2)", "[ 1, 2 ]"),
    ( "Maybe.Just",
      "[Function: Just]"
    ),
    ( "Maybe.Just 1",
      "{ type: 'Just', vars: [ 1 ] }"
    ),
    ( "Maybe.Nothing",
      "{ type: 'Nothing', vars: [] }"
    ),
    ("True == True", "true"),
    ("2 + 2", "4"),
    ("10 - 2", "8"),
    ( "\"dog\" ++ \"log\"",
      "doglog"
    ),
    ( "{ fn: (\\a -> let d = 1 in a) }",
      "{ fn: [Function: fn] }"
    ),
    ( "[1,2] <> [3,4]",
      "[ 1, 2, 3, 4 ]"
    ),
    ( "match Maybe.Just True with (Maybe.Just a) -> a | _ -> False",
      "true"
    ),
    ( "match Maybe.Just True with (Maybe.Just a) -> Maybe.Just a | _ -> Maybe.Nothing",
      "{ type: 'Just', vars: [ true ] }"
    ),
    ( "match Maybe.Just True with (Maybe.Just a) -> let b = 1; Maybe.Just a | _ -> Maybe.Nothing",
      "{ type: 'Just', vars: [ true ] }"
    ),
    ( "let (a, b) = (1,2) in a",
      "1"
    ),
    ( "let { dog: a, cat: b } = { dog: 1, cat: 2} in (a,b)",
      "[ 2, 1 ]"
    ),
    ("let str = \"hey\" in match (Maybe.Just str) with (Maybe.Just a) -> a | _ -> \"\"", "hey"),
    ("\"hello world\"", "hello world"),
    ("Either.Right 101", "{ type: 'Right', vars: [ 101 ] }"),
    ("let stringReduce a = 100 in stringReduce", "[Function: main]"),
    ("let const = True; 1", "1"),
    ("2 > 1", "true"),
    ("1 > 2", "false"),
    ("1 >= 1", "true"),
    ("0 >= 1", "false"),
    ("1 < 2", "true"),
    ("2 < 1", "false"),
    ("2 <= 2", "true"),
    ("3 <= 2", "false"),
    ("let and a b = if a then b else False; let a = 1; let b = 3; let c = 6; and False True", "false"),
    ("\"\nHello\n\"", "\nHello\n"),
    ("match Either.Right 1 with Either.Right a -> a | _ -> 0", "1")
  ]

spec :: Spec
spec = do
  describe "Typescript" $ do
    describe "from parsed input" $ do
      traverse_ testIt testCases

      it "simple expression" $ do
        testFromInputText "{ dog: \\a -> a + 100 }"
          `shouldBe` Right "export const main = { dog: (a: number) => a + 100 }"

      -- what the fuck
      it "pattern matching array spreads" $ do
        testFromInputText "\\a -> match a with [a1,...as] -> [as] | [] -> []"
          `shouldBe` Right "export const main = <D>(a: D[]) => { const match = (value: D[]): D[][] => { if (value.length >= 1) { const [a1,...as] = value; return [as]; }; if (value.length === 0) { return []; }; throw new Error(\"Pattern match error\"); }; return match(a); }"

    describe "Entire compilation" $ do
      traverse_ fullTestIt fullTestCases

    describe "Compile `main` function from modules and run them in Node" $ do
      let moduleTestCases =
            [ ( joinLines
                  [ "export def main = 1 + 2"
                  ],
                "3"
              ),
              ( joinLines
                  [ "def adding a b = a + b",
                    "infix +++ = adding",
                    "export def main = 1 +++ 2"
                  ],
                "3"
              ),
              ( joinLines
                  [ "export type Identity a = Identity a",
                    "def runIdentity a = let (Identity inner) = a in inner",
                    "export def main = runIdentity (Identity True)"
                  ],
                "true"
              ),
              ( joinLines
                  [ "export type Either e a = Left e | Right a",
                    "def useEither val = match val with Right a -> a | _ -> False",
                    "def shouldHaveEitherAsDep val = useEither val",
                    "export def main = shouldHaveEitherAsDep (Right True)"
                  ],
                "true"
              )
            ]
       in traverse_ testModule moduleTestCases

    describe "Compile and open entire project" $ do
      xit "Compiles entire project" $ do
        (filename, contentHash) <- testWholeProjectCompile "CompileTSProjectWhole" stdlib Typescript
        cachePath <- createOutputFolder "CompileTSProjectWhole-result"
        let cacheFilename = cachePath <> show contentHash <> ".json"

        result <- withCache cacheFilename (testTypescriptFileInNode filename)
        result `shouldSatisfy` const True
