{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main
  ( main,
  )
where

-- import qualified Data.Aeson as JSON
import Test.Hspec
import qualified Test.Interpreter as Interpreter
import Test.QuickCheck.Instances ()
import qualified Test.Resolver as Resolver
import qualified Test.Substitutor as Substitutor
import qualified Test.Syntax as Syntax
import qualified Test.Typechecker as Typechecker

main :: IO ()
main = hspec $ do
  Syntax.spec
  Interpreter.spec
  Resolver.spec
  Substitutor.spec
  Typechecker.spec
