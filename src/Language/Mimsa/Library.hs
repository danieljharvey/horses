{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Library
  ( libraryFunctions,
    getLibraryFunction,
    isLibraryName,
  )
where

import Data.Coerce
import qualified Data.Map as M
import Language.Mimsa.Types
import System.Random

libraryFunctions :: Library
libraryFunctions =
  Library $ M.singleton (FuncName "randomInt") getRandom

isLibraryName :: Name -> Bool
isLibraryName name =
  M.member (coerce name) (getLibrary libraryFunctions)

getLibraryFunction :: Name -> Maybe (MonoType, IO Expr)
getLibraryFunction name =
  M.lookup (coerce name) (getLibrary libraryFunctions)

getRandom :: (MonoType, IO Expr)
getRandom = (mt, action)
  where
    mt = MTInt
    action = MyLiteral <$> MyInt <$> randomIO
