{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Library
  ( libraryFunctions,
    getLibraryFunction,
    isLibraryName,
    getFFType,
    reduce,
  )
where

import Data.Coerce
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Map as M
import qualified Data.Text.IO as T
import Language.Mimsa.Types
import System.Random

libraryFunctions :: Library
libraryFunctions =
  Library $
    M.fromList
      [ (FuncName "randomInt", randomInt),
        (FuncName "randomIntFrom", randomIntFrom),
        (FuncName "incrementInt", incrementInt),
        (FuncName "eq", eq),
        (FuncName "log", logFn),
        (FuncName "appendList", appendList),
        (FuncName "reduceList", reduceList),
        (FuncName "addInt", addInt),
        (FuncName "mapList", mapList),
        (FuncName "foldList", foldList)
      ]

isLibraryName :: Name -> Bool
isLibraryName name =
  M.member (coerce name) (getLibrary libraryFunctions)

getLibraryFunction :: Variable -> Maybe ForeignFunc
getLibraryFunction (BuiltIn name) =
  M.lookup (coerce name) (getLibrary libraryFunctions)
getLibraryFunction (NamedVar name) =
  M.lookup (coerce name) (getLibrary libraryFunctions)
getLibraryFunction (BuiltInActual name _) =
  M.lookup (coerce name) (getLibrary libraryFunctions)
getLibraryFunction _ = Nothing

logFn :: ForeignFunc
logFn =
  let tyA = MTVar (NamedVar (Name "a"))
   in OneArg (tyA, MTUnit) logExpr

logExpr :: (Printer p) => p -> IO (Expr a)
logExpr i = do
  T.putStrLn (prettyPrint i)
  pure (MyLiteral MyUnit)

randomInt :: ForeignFunc
randomInt = NoArgs MTInt action
  where
    action = MyLiteral . MyInt <$> randomIO

randomIntFrom :: ForeignFunc
randomIntFrom =
  OneArg
    (MTInt, MTInt)
    ( \(MyLiteral (MyInt i)) -> do
        val <- randomIO
        pure (MyLiteral (MyInt (max val i)))
    )

incrementInt :: ForeignFunc
incrementInt =
  OneArg
    (MTInt, MTInt)
    (\(MyLiteral (MyInt i)) -> pure (MyLiteral (MyInt (i + 1))))

eq :: ForeignFunc
eq =
  let tyA = MTVar (NamedVar (Name "a"))
   in TwoArgs
        (tyA, tyA, MTBool)
        equality

equality :: (Monad m, Eq a) => Expr a -> Expr a -> m (Expr a)
equality x y = pure $ MyLiteral (MyBool (x == y))

addInt :: ForeignFunc
addInt =
  TwoArgs
    (MTInt, MTInt, MTInt)
    ( \(MyLiteral (MyInt a))
       (MyLiteral (MyInt b)) -> pure (MyLiteral (MyInt (a + b)))
    )

appendList :: ForeignFunc
appendList =
  TwoArgs
    (listType, listType, listType)
    (\(MyList listA) (MyList listB) -> pure $ MyList (listA <> listB))
  where
    listType = MTList (MTVar (NamedVar (Name "a")))

reduceList :: ForeignFunc
reduceList =
  let tyA = MTVar (NamedVar (Name "a"))
      tyB = MTVar (NamedVar (Name "b"))
      funcType = MTFunction tyA (MTFunction tyB tyB)
   in ThreeArgs
        (funcType, tyB, MTList tyA, tyB)
        (\f starting (MyList as) -> pure $ reduce f starting as)

reduce :: Expr Variable -> Expr Variable -> NonEmpty (Expr Variable) -> Expr Variable
reduce f starting as =
  let varF = NamedVar (Name "f")
      result = foldr (MyApp . MyApp (MyVar varF)) starting as
   in MyLet varF f result

mapList :: ForeignFunc
mapList =
  let tyA = MTVar (NamedVar (Name "a"))
      tyB = MTVar (NamedVar (Name "b"))
      tyAToB = MTFunction tyA tyB
   in TwoArgs
        (tyAToB, MTList tyA, MTList tyB)
        (\aToB (MyList as) -> pure $ MyList (MyApp aToB <$> as))

foldList :: ForeignFunc
foldList =
  let tyA = MTVar (NamedVar (Name "a"))
      tyAppend = MTFunction tyA (MTFunction tyA tyA)
   in TwoArgs
        (tyAppend, MTList tyA, tyA)
        (\append (MyList as) -> pure $ foldr1 (MyApp . MyApp append) as)

getFFType :: ForeignFunc -> MonoType
getFFType (NoArgs out _) = out
getFFType (OneArg (in1, out) _) = MTFunction in1 out
getFFType (TwoArgs (in1, in2, out) _) = MTFunction in1 (MTFunction in2 out)
getFFType (ThreeArgs (in1, in2, in3, out) _) = MTFunction in1 (MTFunction in2 (MTFunction in3 out))
