{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main,
  )
where

import Criterion.Main
import Data.Functor
import Data.Text (Text)
import qualified Data.Text as T
import qualified Language.Mimsa.Actions.Compile as Actions
import qualified Language.Mimsa.Actions.Evaluate as Actions
import qualified Language.Mimsa.Actions.Monad as Actions
import Language.Mimsa.Backend.Types
import Language.Mimsa.Parser
import Language.Mimsa.Printer
import Language.Mimsa.Project.Stdlib
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Project

unsafeParseExpr :: Text -> Expr Name ()
unsafeParseExpr t = case parseExpr t of
  Right a -> a $> ()
  Left _ ->
    error $
      "Error parsing expr for benchmark:"
        <> T.unpack t

-- build the stdlib, exploding if it fails (so that doesn't look like an
-- incredible speedup)
buildThing ::
  Actions.ActionM () ->
  Project Annotation ->
  Project Annotation
buildThing action prj =
  case Actions.run prj action of
    Right (proj, _, _) -> proj
    Left e -> error (show e)

-- compile something
compileThing :: Text -> Project Annotation
compileThing input =
  let action = do
        let expr = unsafeParseExpr input $> mempty
        (_, _, storeExpr, _, _) <- Actions.evaluate (prettyPrint expr) expr
        Actions.compile Typescript storeExpr
   in case Actions.run stdlib action of
        Right (proj, _, _) -> proj
        Left e -> error (show e)

-- evaluate something
evaluateThing :: Text -> Expr Name Annotation
evaluateThing input =
  let action = do
        let expr = unsafeParseExpr input $> mempty
        (_, result, _, _, _) <- Actions.evaluate (prettyPrint expr) expr
        pure result
   in case Actions.run stdlib action of
        Right (_, _, res) -> res
        Left e -> error (show e)

-- Our benchmark harness.
main :: IO ()
main =
  defaultMain
    [ bgroup
        "build stdlib"
        [ bench "allFns" $ whnf (buildThing allFns) mempty
        ],
      bgroup
        "compilation"
        [ bench "compile either fmap test" $
            whnf compileThing "either.fmap (\\a -> a + 1) (Right 100)"
        ],
      bgroup
        "evaluate"
        [ bench "evaluate big looping thing" $
            whnf evaluateThing "let countdown a = if a == 0 then True else countdown (a - 1); countdown 100000",
          bench "evaluate parsing" $
            whnf evaluateThing "let pA = parser.char \"a\"; let pB = parser.char \"b\"; let p = parser.many (parser.alt pA pB); parser.run p \"aababaa\"",
          bench "evaluate parsing 2" $
            let input = T.replicate 1000 ",d"
             in whnf
                  evaluateThing
                  ( mconcat
                      [ "let lexeme p = parser.left p parser.space0; ",
                        "let bracketL = lexeme (parser.char \"[\"); ",
                        "let bracketR = lexeme (parser.char \"]\"); ",
                        "let comma = lexeme (parser.char \",\"); ",
                        "let inner = lexeme (parser.char \"d\"); ",
                        "let bigP = parser.right bracketL (parser.left (parser.sepBy comma inner) bracketR); ",
                        "parser.run bigP \"[d" <> input <> "]\""
                      ]
                  )
        ]
    ]
