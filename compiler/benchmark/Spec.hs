{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main,
  )
where

import Criterion.Types (Config(..))
import Criterion.Main
import Data.Functor
import Data.Text (Text)
import qualified Data.Text as T
import qualified Language.Mimsa.Actions.Modules.Evaluate as Actions
import qualified Language.Mimsa.Actions.Monad as Actions
import Language.Mimsa.Core
import Language.Mimsa.Project.Stdlib
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

-- evaluate something
evaluateThing :: Text -> Expr Name Annotation
evaluateThing input =
  let action = do
        let expr = unsafeParseExpr input $> mempty
        (_, result, _) <- Actions.evaluateModule expr mempty
        pure result
   in case Actions.run stdlib action of
        Right (_, _, res) -> res
        Left e -> error (show e)

benchConfig :: Config
benchConfig = defaultConfig { jsonFile = Just "performance.json"}

-- Our benchmark harness.
main :: IO ()
main =
  defaultMainWith benchConfig
    [ bgroup
        "build stdlib"
        [ bench "allFns" $ whnf (buildThing stdModules) mempty
        ],
      bgroup
        "evaluate"
        [ bench "evaluate big looping thing" $
            whnf evaluateThing "let countdown a = if a == 0 then True else countdown (a - 1); countdown 100000",
          bench "evaluate parsing" $
            whnf evaluateThing "let pA = Parser.char \"a\"; let pB = Parser.char \"b\"; let p = Parser.many (Parser.alt pA pB); Parser.run p \"aababaa\"",
          bench "evaluate parsing 2" $
            let input = T.replicate 1000 ",d"
             in whnf
                  evaluateThing
                  ( mconcat
                      [ "let lexeme p = Parser.left p Parser.space0; ",
                        "let bracketL = lexeme (Parser.char \"[\"); ",
                        "let bracketR = lexeme (Parser.char \"]\"); ",
                        "let comma = lexeme (Parser.char \",\"); ",
                        "let inner = lexeme (Parser.char \"d\"); ",
                        "let bigP = Parser.right bracketL (Parser.left (Parser.sepBy comma inner) bracketR); ",
                        "Parser.run bigP \"[d" <> input <> "]\""
                      ]
                  )
        ]
    ]
