{-# LANGUAGE OverloadedStrings #-}

module Test.MonoTypeParser
  ( spec,
  )
where

import Control.Monad.Except
import Data.Functor
import qualified Data.Map as M
import Data.Text (Text)
import Language.Mimsa.Parser.Helpers
import Language.Mimsa.Parser.MonoType
import Language.Mimsa.Printer
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Typechecker
import Test.Hspec
import Test.Utils.Helpers

testParser :: Text -> Either Text MonoType
testParser input = do
  let removeAnn mt = mt $> mempty
      doParser i =
        removeAnn <$> parseAndFormat monoTypeParser i
  original <- doParser input
  newOne <- doParser (prettyPrint original)
  if newOne == original
    then pure newOne
    else throwError $ "Error! " <> prettyPrint newOne <> " does not equal " <> prettyPrint original

spec :: Spec
spec =
  fdescribe "MonoType parser" $ do
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
      testParser "(Int,     String)"
        `shouldBe` Right
          (MTPair mempty (MTPrim mempty MTInt) (MTPrim mempty MTString))
    it "Pair with less spacing" $
      testParser "(a,b) ->   a"
        `shouldBe` Right
          ( MTFunction
              mempty
              (MTPair mempty (MTVar mempty (tvNamed "a")) (MTVar mempty (tvNamed "b")))
              (MTVar mempty (tvNamed "a"))
          )
    it "Function with pair" $
      testParser "(Int, String) -> Int"
        `shouldBe` Right
          ( MTFunction
              mempty
              (MTPair mempty (MTPrim mempty MTInt) (MTPrim mempty MTString))
              (MTPrim mempty MTInt)
          )
    it "Function with variables" $
      testParser "a -> a"
        `shouldBe` Right
          ( MTFunction
              mempty
              (MTVar mempty (tvNamed "a"))
              (MTVar mempty (tvNamed "a"))
          )
    it "Empty record" $
      testParser "{}"
        `shouldBe` Right (MTRecord mempty mempty)
    it "Record with items" $
      testParser "{one:Int,two:String}"
        `shouldBe` Right
          ( MTRecord mempty $
              M.fromList
                [ (mkName "one", MTPrim mempty MTInt),
                  (mkName "two", MTPrim mempty MTString)
                ]
          )
    it "Record with functions as items" $
      testParser "{ one: Int -> Int, two: String -> b }"
        `shouldBe` Right
          ( MTRecord mempty $
              M.fromList
                [ ( mkName "one",
                    MTFunction
                      mempty
                      (MTPrim mempty MTInt)
                      (MTPrim mempty MTInt)
                  ),
                  ( mkName "two",
                    MTFunction
                      mempty
                      (MTPrim mempty MTString)
                      (MTVar mempty (tvNamed "b"))
                  )
                ]
          )
    it "Record with all sorts of stuff in it" $
      testParser "{ one: Int -> Maybe Int, two: String -> (b, Either String Int) }"
        `shouldBe` Right
          ( MTRecord mempty $
              M.fromList
                [ ( mkName "one",
                    MTFunction
                      mempty
                      (MTPrim mempty MTInt)
                      ( MTData
                          mempty
                          (mkTyCon "Maybe")
                          [MTPrim mempty MTInt]
                      )
                  ),
                  ( mkName "two",
                    MTFunction
                      mempty
                      (MTPrim mempty MTString)
                      ( MTPair
                          mempty
                          (MTVar mempty (tvNamed "b"))
                          ( MTData
                              mempty
                              (mkTyCon "Either")
                              [ MTPrim mempty MTString,
                                MTPrim mempty MTInt
                              ]
                          )
                      )
                  )
                ]
          )
    it "Nullary data type" $
      testParser "MyUnit"
        `shouldBe` Right
          (MTData mempty (mkTyCon "MyUnit") mempty)
    it "Unary data type" $
      testParser "Maybe String"
        `shouldBe` Right
          ( MTData
              mempty
              (mkTyCon "Maybe")
              [MTPrim mempty MTString]
          )
    it "Binary data type" $
      testParser "Either String Int"
        `shouldBe` Right
          ( MTData
              mempty
              (mkTyCon "Either")
              [ MTPrim mempty MTString,
                MTPrim mempty MTInt
              ]
          )
    it "Binary data type with sub type" $
      testParser "Either String (Maybe Int)"
        `shouldBe` Right
          ( MTData
              mempty
              (mkTyCon "Either")
              [ MTPrim mempty MTString,
                MTData
                  mempty
                  (mkTyCon "Maybe")
                  [MTPrim mempty MTInt]
              ]
          )
    it "Functions with datatypes 1" $
      testParser "MyUnit -> Int"
        `shouldBe` Right
          ( MTFunction
              mempty
              (MTData mempty (mkTyCon "MyUnit") mempty)
              (MTPrim mempty MTInt)
          )
    it "Functions with datatypes 2" $
      testParser "Maybe String -> Int"
        `shouldBe` Right
          ( MTFunction
              mempty
              (MTData mempty (mkTyCon "Maybe") [MTPrim mempty MTString])
              (MTPrim mempty MTInt)
          )
    it "Parses higher order function" $
      testParser "(a -> b) -> a -> b"
        `shouldBe` Right
          ( MTFunction
              mempty
              (MTFunction mempty (typeName "a") (typeName "b"))
              ( MTFunction
                  mempty
                  (typeName "a")
                  (typeName "b")
              )
          )
    it "Parses fmap" $
      testParser "(a -> b) -> Option a -> Option b"
        `shouldBe` Right
          ( MTFunction
              mempty
              (MTFunction mempty (typeName "a") (typeName "b"))
              ( MTFunction
                  mempty
                  (MTData mempty (mkTyCon "Option") [typeName "a"])
                  (MTData mempty (mkTyCon "Option") [typeName "b"])
              )
          )
