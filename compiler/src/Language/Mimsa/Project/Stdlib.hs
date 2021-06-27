{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Project.Stdlib (buildStdlib, stdlib) where

import Data.Functor
import Data.Text (Text)
import qualified Language.Mimsa.Actions.BindExpression as Actions
import qualified Language.Mimsa.Actions.BindType as Actions
import qualified Language.Mimsa.Actions.Monad as Actions
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
      addType "type Void"
      addType "type Maybe a = Just a | Nothing"
      addType "type Either e a = Left e | Right a"
      addType "type Task r a = Task ((a -> r) -> r)"
      addType "type Parser a  = Parser (String -> Maybe (a, String))"
      addType "type Unit = Unit"
      addBinding "id" "\\a -> a"
      addBinding "compose" "\\f -> \\g -> \\a -> f(g(a))"
      addBinding "runParser" "\\parser -> \\str -> match parser with (Parser p) -> match p(str) with  (Just (\"\", a)) -> (Just a) | _ -> (Nothing)"
      addBinding "fmapParser" "\\f -> \\parser -> Parser (\\str -> match parser with (Parser p) -> (let outcome = p(str); match outcome with (Just (a, rest)) -> (Just ((f(a),rest))) | _ -> (Nothing)))"
      addBinding "bindParser" "\\f -> \\parser -> Parser (\\str -> match parser with (Parser p) -> match p(str) with (Just (a, rest)) -> (let nextParser = f(a); match nextParser with (Parser b) -> b(rest)) | _ -> (Nothing))"
      addBinding "charParser" "Parser (\\s -> match s with ch ++ rest -> (Just ((rest, ch))) | _ -> (Nothing))"

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

fromRight :: (Show e) => Either e a -> a
fromRight = \case
  Left e -> error (show e)
  Right a -> a

stdlib :: Project Annotation
stdlib = fromRight buildStdlib
