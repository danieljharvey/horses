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
        (FuncName "eqInt", eqInt),
        (FuncName "eqBool", eqBool),
        (FuncName "eqString", eqString),
        (FuncName "logInt", logInt),
        (FuncName "logString", logString),
        (FuncName "logBool", logBool),
        (FuncName "appendList", appendList),
        (FuncName "reduceList", reduceList),
        (FuncName "addInt", addInt)
      ]

isLibraryName :: Name -> Bool
isLibraryName name =
  M.member (coerce name) (getLibrary libraryFunctions)

getLibraryFunction :: Variable -> Maybe ForeignFunc
getLibraryFunction (BuiltIn name) =
  M.lookup (coerce name) (getLibrary libraryFunctions)
getLibraryFunction (NamedVar name) =
  M.lookup (coerce name) (getLibrary libraryFunctions)
getLibraryFunction _ = Nothing

logInt :: ForeignFunc
logInt = OneArg (MTInt, MTUnit) logExpr

logString :: ForeignFunc
logString = OneArg (MTString, MTUnit) logExpr

logBool :: ForeignFunc
logBool = OneArg (MTBool, MTUnit) logExpr

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

eqInt :: ForeignFunc
eqInt =
  TwoArgs
    (MTInt, MTInt, MTBool)
    equality

eqBool :: ForeignFunc
eqBool =
  TwoArgs
    (MTBool, MTBool, MTBool)
    equality

eqString :: ForeignFunc
eqString =
  TwoArgs
    (MTString, MTString, MTBool)
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
  let tyB = MTVar (NamedVar (Name "b"))
      tyA = MTVar (NamedVar (Name "a"))
      funcType = MTFunction tyB (MTFunction tyA tyB)
   in ThreeArgs
        (funcType, tyB, MTList tyA, tyB)
        (\f starting (MyList as) -> pure $ reduce f starting as)

reduce :: Expr Variable -> Expr Variable -> NonEmpty (Expr Variable) -> Expr Variable
reduce f starting as =
  let varF = NamedVar (Name "f")
      result = foldl (\b' a' -> MyApp (MyApp (MyVar varF) b') a') starting as
   in MyLet varF f result

getFFType :: ForeignFunc -> MonoType
getFFType (NoArgs out _) = out
getFFType (OneArg (in1, out) _) = MTFunction in1 out
getFFType (TwoArgs (in1, in2, out) _) = MTFunction in1 (MTFunction in2 out)
getFFType (ThreeArgs (in1, in2, in3, out) _) = MTFunction in1 (MTFunction in2 (MTFunction in3 out))
