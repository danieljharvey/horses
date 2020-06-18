{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Library
  ( libraryFunctions,
    isLibraryName,
  )
where

import Data.Coerce
import Data.Map (Map)
import qualified Data.Map as M
import Language.Mimsa.Types
import System.Random

libraryFunctions :: Map FuncName (MonoType, IO Expr)
libraryFunctions = M.singleton (FuncName "randomInt") getRandom

isLibraryName :: Name -> Bool
isLibraryName name = M.member (coerce name) libraryFunctions

getRandom :: (MonoType, IO Expr)
getRandom = (mt, action)
  where
    mt = MTInt
    action = MyLiteral <$> MyInt <$> randomIO
