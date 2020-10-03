{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Library
  ( libraryFunctions,
    getLibraryFunction,
    isLibraryName,
    getFFType,
  )
where

import Data.Coerce
import qualified Data.Map as M
import qualified Data.Text.IO as T
import Language.Mimsa.Printer
import Language.Mimsa.Types
import System.Random

type ForeignFunc' = ForeignFunc ()

libraryFunctions :: Library ()
libraryFunctions =
  Library $
    M.fromList
      [ (FuncName "randomInt", randomInt),
        (FuncName "randomIntFrom", randomIntFrom),
        (FuncName "eqPair", eqPair),
        (FuncName "log", logFn),
        (FuncName "addIntPair", addIntPair)
      ]

isLibraryName :: Name -> Bool
isLibraryName name =
  M.member (coerce name) (getLibrary libraryFunctions)

getLibraryFunction :: Variable -> Maybe ForeignFunc'
getLibraryFunction (BuiltIn name) =
  M.lookup (coerce name) (getLibrary libraryFunctions)
getLibraryFunction (NamedVar name) =
  M.lookup (coerce name) (getLibrary libraryFunctions)
getLibraryFunction (BuiltInActual name _) =
  M.lookup (coerce name) (getLibrary libraryFunctions)
getLibraryFunction _ = Nothing

logFn :: ForeignFunc'
logFn =
  let tyA = MTVar (NamedVar (Name "a"))
   in OneArg (tyA, MTUnit) logExpr
  where
    logExpr i = do
      T.putStrLn (prettyPrint i)
      pure (MyLiteral () MyUnit)

randomInt :: ForeignFunc'
randomInt = NoArgs MTInt action
  where
    action = MyLiteral () . MyInt <$> randomIO

randomIntFrom :: ForeignFunc'
randomIntFrom =
  OneArg
    (MTInt, MTInt)
    ( \(MyLiteral _ (MyInt i)) -> do
        val <- randomIO
        pure (MyLiteral () (MyInt (max val i)))
    )

eqPair :: ForeignFunc'
eqPair =
  let tyA = MTVar (NamedVar (Name "a"))
   in OneArg
        (MTPair tyA tyA, MTBool)
        equality
  where
    equality pair' =
      let (MyPair _ x y) = pair'
       in pure $ MyLiteral () (MyBool (x == y))

addIntPair :: ForeignFunc'
addIntPair =
  OneArg
    (MTPair MTInt MTInt, MTInt)
    ( \( MyPair
           _
           (MyLiteral _ (MyInt a))
           (MyLiteral _ (MyInt b))
         ) -> pure (MyLiteral () (MyInt (a + b)))
    )

getFFType :: ForeignFunc' -> MonoType
getFFType (NoArgs out _) = out
getFFType (OneArg (in1, out) _) = MTFunction in1 out
