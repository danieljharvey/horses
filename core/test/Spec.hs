module Main
  ( main,
  )
where

import Test.Hspec
import qualified Test.Parser.DataTypes
import qualified Test.Parser.MonoTypeParser
import qualified Test.Parser.Pattern
import qualified Test.Parser.Syntax
import qualified Test.Prettier

main :: IO ()
main =
  hspec $ do
    Test.Parser.DataTypes.spec
    Test.Parser.MonoTypeParser.spec
    Test.Parser.Pattern.spec
    Test.Parser.Syntax.spec
    Test.Prettier.spec
