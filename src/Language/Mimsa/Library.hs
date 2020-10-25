{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

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

libraryFunctions :: (Eq ann, Monoid ann) => Library ann
libraryFunctions =
  Library $
    M.fromList
      [ (FuncName "randomInt", randomInt),
        (FuncName "randomIntFrom", randomIntFrom),
        (FuncName "log", logFn),
        (FuncName "addIntPair", addIntPair)
      ]

isLibraryName ::
  forall ann.
  (Eq ann, Monoid ann) =>
  Name ->
  Bool
isLibraryName name =
  M.member (coerce name) (getLibrary @ann libraryFunctions)

getLibraryFunction ::
  (Eq ann, Monoid ann) =>
  Variable ->
  Maybe (ForeignFunc ann)
getLibraryFunction (BuiltIn name) =
  M.lookup (coerce name) (getLibrary libraryFunctions)
getLibraryFunction (NamedVar name) =
  M.lookup (coerce name) (getLibrary libraryFunctions)
getLibraryFunction (BuiltInActual name _) =
  M.lookup (coerce name) (getLibrary libraryFunctions)
getLibraryFunction _ = Nothing

logFn :: (Monoid ann) => ForeignFunc ann
logFn =
  let tyA = MTVar (NamedVar (Name "a"))
   in OneArg (tyA, MTPrim MTUnit) logExpr
  where
    logExpr i = do
      T.putStrLn (prettyPrint i)
      pure (MyLiteral mempty MyUnit)

randomInt :: Monoid ann => ForeignFunc ann
randomInt = NoArgs (MTPrim MTInt) action
  where
    action = MyLiteral mempty . MyInt <$> randomIO

randomIntFrom :: Monoid ann => ForeignFunc ann
randomIntFrom =
  OneArg
    (MTPrim MTInt, MTPrim MTInt)
    ( \(MyLiteral _ (MyInt i)) -> do
        val <- randomIO
        pure (MyLiteral mempty (MyInt (max val i)))
    )

addIntPair :: Monoid ann => ForeignFunc ann
addIntPair =
  OneArg
    (MTPair (MTPrim MTInt) (MTPrim MTInt), MTPrim MTInt)
    ( \( MyPair
           _
           (MyLiteral _ (MyInt a))
           (MyLiteral _ (MyInt b))
         ) -> pure (MyLiteral mempty (MyInt (a + b)))
    )

getFFType :: ForeignFunc ann -> MonoType
getFFType (NoArgs out _) = out
getFFType (OneArg (in1, out) _) = MTFunction in1 out
