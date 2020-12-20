{-# LANGUAGE OverloadedStrings #-}

module Test.MonoTypeParser
  ( spec,
  )
where

import Data.Functor
import Data.Text (Text)
import Language.Mimsa.Parser.Helpers
import Language.Mimsa.Parser.MonoType
import Language.Mimsa.Types.Typechecker
import Test.Hspec
import Test.Utils.Helpers
import Text.Megaparsec

testParser :: Text -> Either Text MonoType
testParser input =
  removeAnn
    <$> parseAndFormat (monoTypeParser <* eof) input
  where
    removeAnn mt =
      mt $> mempty

spec :: Spec
spec =
  describe "MonoType parser" $ do
    it "String" $
      testParser "String" `shouldBe` Right (MTPrim mempty MTString)
    it "Unit" $
      testParser "Unit" `shouldBe` Right (MTPrim mempty MTUnit)
    it "Boolean" $
      testParser "Boolean" `shouldBe` Right (MTPrim mempty MTBool)
    it "Int" $
      testParser "Int" `shouldBe` Right (MTPrim mempty MTInt)
    it "Function with primitives" $
      testParser "Int -> Int"
        `shouldBe` Right
          ( MTFunction
              mempty
              (MTPrim mempty MTInt)
              (MTPrim mempty MTInt)
          )
    it "Pair of primitives" $
      testParser "(Int, String)"
        `shouldBe` Right
          (MTPair mempty (MTPrim mempty MTInt) (MTPrim mempty MTString))
    it "Variable" $
      testParser "A"
        `shouldBe` Right (MTVar mempty (tvNamed "A"))
    it "Function with pair" $
      testParser "(Int, String) -> Int"
        `shouldBe` Right
          ( MTFunction
              mempty
              (MTPair mempty (MTPrim mempty MTInt) (MTPrim mempty MTString))
              (MTPrim mempty MTInt)
          )
