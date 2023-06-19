{-# LANGUAGE OverloadedStrings #-}

module Test.IR.IRSpec (spec) where

import Data.Foldable (traverse_)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified LLVM.AST as LLVM
import qualified Smol.Backend.Compile.RunLLVM as Run
import Smol.Backend.IR.FromExpr.Expr
import Smol.Backend.IR.ToLLVM.ToLLVM
import Smol.Core.Modules.FromParts
import Smol.Core.Modules.ModuleError
import Smol.Core.Modules.ResolveDeps
import Smol.Core.Modules.Typecheck
import Smol.Core.Parser (parseModuleAndFormatError)
import Smol.Core.Types
import Smol.Core.Types.Module
import Test.BuiltInTypes
import Test.Hspec
import Test.IR.Samples

-- run the code, get the output, die
run :: LLVM.Module -> [(String, String)] -> IO Text
run llModule env =
  Run.rrResult <$> Run.run env llModule

createLLVMModuleFromExpr :: Text -> LLVM.Module
createLLVMModuleFromExpr input =
  createLLVMModuleFromModule $ "def main = " <> input

testCompileIR :: (Text, Text) -> Spec
testCompileIR (input, result) = it ("Via IR " <> show input) $ do
  resp <- run (createLLVMModuleFromExpr input) []
  resp `shouldBe` result

createLLVMModuleFromModule :: Text -> LLVM.Module
createLLVMModuleFromModule input =
  case resolveModule input of
    Right typecheckedModule ->
      irToLLVM (irFromModule typecheckedModule)
    Left e -> error (show e)

-- add the empty ones for testing
addTestDataTypesToModule :: (Monoid ann) => Module ParseDep ann -> Module ParseDep ann
addTestDataTypesToModule myModule =
  myModule {moDataTypes = moDataTypes myModule <> M.mapKeys pdIdentifier (builtInTypes emptyParseDep)}

resolveModule :: Text -> Either ModuleError (Module ResolvedDep (Type ResolvedDep Annotation))
resolveModule input =
  case parseModuleAndFormatError input of
    Right moduleParts -> do
      case moduleFromModuleParts mempty moduleParts >>= resolveModuleDeps . addTestDataTypesToModule of
        Left e -> error (show e)
        Right (myModule, deps) -> do
          typecheckModule mempty "" myModule deps
    Left e -> error (show e)

testCompileModuleIR :: ([Text], Text) -> Spec
testCompileModuleIR (inputs, result) =
  let input = T.intercalate "\n" inputs
   in it ("Via IR " <> show input) $ do
        resp <- run (createLLVMModuleFromModule input) []
        resp `shouldBe` result

spec :: Spec
spec = do
  describe "Compile via IR" $ do
    describe "IR" $ do
      it "print 42" $ do
        resp <- run (irToLLVM irPrint42) mempty
        resp `shouldBe` "42"
      it "use id function" $ do
        resp <- run (irToLLVM irId42) mempty
        resp `shouldBe` "42"
      it "creates and destructures tuple" $ do
        resp <- run (irToLLVM irTwoTuple42) mempty
        resp `shouldBe` "42"
      it "does an if statement" $ do
        resp <- run (irToLLVM irBasicIf) mempty
        resp `shouldBe` "42"
      it "does a pattern match" $ do
        resp <- run (irToLLVM irPatternMatch) mempty
        resp `shouldBe` "42"
      it "recursive function" $ do
        resp <- run (irToLLVM irRecursive) mempty
        resp `shouldBe` "49995000"
      it "curried function (no closure)" $ do
        resp <- run (irToLLVM irCurriedNoClosure) mempty
        resp `shouldBe` "22"
      it "curried function" $ do
        resp <- run (irToLLVM irCurried) mempty
        resp `shouldBe` "42"

    describe "From modules" $ do
      let testModules =
            [ ( [ "def one = 1",
                  "def main = one + one"
                ],
                "2"
              ),
              ( [ "def increment a = (a + 1 : Int)",
                  "def main = increment 41"
                ],
                "42"
              ),
              ( [ "def add (a: Nat) (b : Nat): Nat = a + b",
                  "def main = add 20 22"
                ],
                "42"
              ),
              ( [ "type Identity a = Identity a",
                  "def main = case Identity 42 of Identity a -> a"
                ],
                "42"
              ),
              ( [ "type Identity a = Identity a",
                  "def main = case Identity (41 + 1) of Identity a -> a"
                ],
                "42"
              ),
              ( [ "type Identity a = Identity a",
                  "def main = let id = (\\a -> a : Int -> Int); case Identity (id 42) of Identity a -> a"
                ],
                "42"
              ),
              ( [ "type Identity a = Identity a",
                  "def runIdentity (identA: Identity Int): Int = case identA of Identity b -> b",
                  "def main = runIdentity (Identity 42)"
                ],
                "42"
              )
            ]
      describe "IR compile" $ do
        traverse_ testCompileModuleIR testModules

    describe "From expressions" $ do
      describe "Basic" $ do
        let testVals =
              [ ("42", "42"),
                ("True", "True"),
                ("False", "False"),
                ("(1 + 1 : Int)", "2"),
                ("(1 + 2 + 3 + 4 + 5 + 6 : Int)", "21"),
                ("(if True then 1 else 2 : Nat)", "1"),
                ("(if False then 1 else 2 : Nat)", "2"),
                ("\"horse\"", "horse"),
                ("if True then \"horse\" else \"no-horse\"", "horse"),
                ("\"hor\" + \"se\"", "horse"),
                ("\"horse\" == \"horse\"", "True"),
                ("\"hor\" + \"se\" == \"horse\"", "True"),
                ("(\"dog\" : String) == (\"log\" : String)", "False"),
                ("case \"dog\" of \"dog\" -> True | _ -> False", "True"),
                ("case (\"log\" : String) of \"dog\" -> True | _ -> False", "False"),
                ("case \"dog\" of \"dog\" -> True | _ -> False", "True")
              ]

        describe "IR compile" $ do
          traverse_ testCompileIR testVals

      describe "Functions" $ do
        let testVals =
              [ ("(\\a -> a + 1 : Nat -> Nat) 2", "3"),
                ("(\\b -> if b then 42 else 41 : Bool -> Nat) True", "42"),
                ("(\\b -> if b then 1 else 42 : Bool -> Nat) False", "42"),
                ("(\\a -> a + 1: Nat -> Nat) 41", "42"),
                ("(\\a -> 42 : Nat -> Nat) 21", "42"),
                ("(\\a -> \\b -> a + b : Nat -> Nat -> Nat) 20 22", "42"),
                ("let a = (1 : Nat); let useA = (\\b -> b + a : Nat -> Nat); useA (41 : Nat)", "42"),
                ("let add = (\\a -> \\b -> a + b : Nat -> Nat -> Nat); add (1 : Nat) (2 : Nat)", "3"),
                ("let f = (\\i -> i + 1 : Nat -> Nat) in f (1 : Nat)", "2"), -- single arity function that return prim
                ("let f = (\\i -> (i,i) : Nat -> (Nat,Nat)); let b = f (1 : Nat); 42", "42"), -- single arity function that returns struct
                ("let f = (\\i -> (i,10) : Nat -> (Nat,Nat)) in (case f (100 : Nat) of (a,b) -> a + b : Nat)", "110"), -- single arity function that returns struct
                ("let flipConst = (\\a -> \\b -> b : Nat -> Nat -> Nat); flipConst (1 : Nat) (2 : Nat)", "2") -- oh fuck
                -- ("let sum = (\\a -> if a == 10 then 0 else let a2 = a + 1 in a + sum a2 : Nat -> Nat); sum (0 : Nat)", "1783293664"),
                -- ("let add3 = (\\a -> \\b -> \\c -> a + b + c : Nat -> Nat -> Nat -> Nat); add3 (1 : Nat) (2 : Nat) (3 : Nat)", "6"),
              ]

        describe "IR compile" $ do
          traverse_ testCompileIR testVals

      describe "Tuples and matching" $ do
        let testVals =
              [ ("let pair = (20,22); (case pair of (a,b) -> a + b : Nat)", "42"),
                ("(\\pair -> case pair of (a,b) -> a + b : (Nat,Nat) -> Nat) (20,22)", "42"),
                ("(\\triple -> case triple of (a,b,c) -> a + b + c : (Nat,Nat,Nat) -> Nat) (20,11,11)", "42"),
                ("(\\bool -> case bool of True -> 0 | False -> 1 : Bool -> Nat) False", "1"),
                ("(\\bools -> case bools of (True,_) -> 0 | (False,_) -> 1 : (Bool,Bool) -> Nat) (False,False)", "1")
              ]

        describe "IR compile" $ do
          traverse_ testCompileIR testVals

      describe "Arrays and matching" $ do
        let testVals =
              [ ("let arr = [20,22]; case arr of [a,b] -> (a + b : Int) | _ -> 0", "42"),
                ("let arr = [20,20,2]; case arr of [a,b,c] -> (a + b + c : Int) | _ -> 0", "42"),
                ("let arr = [1,100]; case arr of [100, a] -> 0 | [1,b] -> b | _ -> 0", "100"),
                ("let arr = [1,2,3]; case arr of [_,_] -> 0 | _ -> 1", "1") -- ie, are we checking the length of the array?
                -- ("let arr1 = [1,2,3]; let arr2 = case arr1 of [_,...rest] -> rest | _ -> [1]; case arr2 of [d,e] -> d + e | _ -> 0", "5") -- need malloc to dynamically create new array
              ]

        describe "IR compile" $ do
          traverse_ testCompileIR testVals

      describe "Datatypes" $ do
        let testVals =
              [ ("(\\ord -> case ord of GT -> 21 | EQ -> 23 | LT -> 42 : Ord -> Nat) LT", "42"), -- constructor with no args
                ("(\\maybe -> case maybe of _ -> 42 : Maybe Nat -> Nat) (Just 41)", "42"),
                ("(\\maybe -> case maybe of Just a -> a + 1 | Nothing -> 0 : Maybe Nat -> Nat) (Just 41)", "42"),
                ("(\\maybe -> case maybe of Just 40 -> 100 | Just a -> a + 1 | Nothing -> 0 : Maybe Nat -> Nat) (Just 41)", "42"), -- predicates in constructor
                ("(\\maybe -> case maybe of Just 40 -> 100 | Just a -> a + 1 | Nothing -> 0 : Maybe Nat -> Nat) (Nothing : Maybe Nat)", "0"), -- predicates in constructor
                ("(\\these -> case these of This aa -> aa | That 27 -> 0 | These a b -> a + b : These Nat Nat -> Nat) (This 42 : These Nat Nat)", "42"), -- data shapes are wrong
                ("(\\these -> case these of This aa -> aa | That 60 -> 0 | These a b -> a + b : These Nat Nat -> Nat) (These 20 22 : These Nat Nat)", "42"),
                -- ("(\\these -> case these of This a -> a | That _ -> 1000 | These a b -> a + b : These Nat Nat -> Nat) (That 42 : These Nat Nat)", "1000"),--wildcards fuck it up for some reason
                ("(case (This 42 : These Nat Nat) of This a -> a : Nat)", "42")
              ]

        describe "IR compile" $ do
          traverse_ testCompileIR testVals

      xdescribe "Nested datatypes (manually split cases)" $ do
        let testVals =
              [ ("let maybe = Just (Just 41) in 42", "42"),
                ("let oneList = Cons 1 Nil in 42", "42"),
                ("let twoList = Cons 1 (Cons 2 Nil) in 42", "42"),
                ("(\\maybe -> case maybe of Just a -> (case a of Just aa -> aa + 1 | _ -> 0) | _ -> 0 : Maybe (Maybe Nat) -> Nat) (Just (Just 41))", "42") -- ,
                -- ("let nested = (20, (11,11)) in 42", "42"),
                -- ("(\\nested -> case nested of (a,(b,c)) -> a + b + c : (Nat, (Nat, Nat)) -> Nat) (20,(11,11))", "42"),
                -- ("(\\maybe -> case maybe of Just (a,b,c) -> a + b + c | Nothing -> 0 : Maybe (Nat,Nat,Nat) -> Nat) (Just (1,2,3))", "6")
              ]

        describe "IR compile" $ do
          traverse_ testCompileIR testVals
      xdescribe "Nested datatypes (currently broken)" $ do
        let testVals =
              [ ("let maybe = Just (Just 41) in 42", "42"),
                ("(\\maybe -> case maybe of Just (Just a) -> a + 1 | _ -> 0 : Maybe (Maybe Nat) -> Nat) (Just (Just 41))", "42"),
                ("let nested = (20, (11,11)) in 42", "42"),
                ("(\\nested -> case nested of (a,(b,c)) -> a + b + c : (Nat, (Nat, Nat)) -> Nat) (20,(11,11))", "42"),
                ("(\\maybe -> case maybe of Just (a,b,c) -> a + b + c | Nothing -> 0 : Maybe (Nat,Nat,Nat) -> Nat) (Just (1,2,3))", "6")
              ]

        describe "IR compile" $ do
          traverse_ testCompileIR testVals
