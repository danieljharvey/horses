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
        (FuncName "eqString", eqString)
      ]

isLibraryName :: Name -> Bool
isLibraryName name =
  M.member (coerce name) (getLibrary libraryFunctions)

getLibraryFunction :: Name -> Maybe ForeignFunc
getLibraryFunction name =
  M.lookup (coerce name) (getLibrary libraryFunctions)

randomInt :: ForeignFunc
randomInt = NoArgs MTInt action
  where
    action = MyLiteral <$> MyInt <$> randomIO

randomIntFrom :: ForeignFunc
randomIntFrom =
  OneArg
    (mkName "minimum", MTInt)
    MTInt
    ( \(MyLiteral (MyInt i)) -> do
        val <- randomIO
        pure (MyLiteral (MyInt (max val i)))
    )

incrementInt :: ForeignFunc
incrementInt =
  OneArg
    (mkName "x", MTInt)
    MTInt
    (\(MyLiteral (MyInt i)) -> pure (MyLiteral (MyInt (i + 1))))

eqInt :: ForeignFunc
eqInt =
  TwoArgs
    (mkName "x", MTInt)
    (mkName "y", MTInt)
    MTBool
    equality

eqBool :: ForeignFunc
eqBool =
  TwoArgs
    (mkName "x", MTBool)
    (mkName "y", MTBool)
    MTBool
    equality

eqString :: ForeignFunc
eqString =
  TwoArgs
    (mkName "x", MTString)
    (mkName "y", MTString)
    MTBool
    equality

equality :: (Monad m) => Expr -> Expr -> m Expr
equality x y = pure $ MyLiteral (MyBool (x == y))

getFFType :: ForeignFunc -> MonoType
getFFType (NoArgs out _) = out
getFFType (OneArg (_, in1) out _) = MTFunction in1 out
getFFType (TwoArgs (_, in1) (_, in2) out _) = MTFunction in1 (MTFunction in2 out)
