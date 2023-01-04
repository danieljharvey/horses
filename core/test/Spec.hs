module Main
  ( main,
  )
where

import Test.Hspec
import qualified CoreTest.Parser.DataTypes
import qualified CoreTest.Parser.MonoTypeParser
import qualified CoreTest.Parser.Pattern
import qualified CoreTest.Parser.Syntax
import qualified CoreTest.Prettier

main :: IO ()
main =
  hspec $ do
    CoreTest.Parser.DataTypes.spec
    CoreTest.Parser.MonoTypeParser.spec
    CoreTest.Parser.Pattern.spec
    CoreTest.Parser.Syntax.spec
    CoreTest.Prettier.spec
