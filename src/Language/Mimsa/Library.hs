{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Library
  ( libraryFunctions,
  )
where

import Data.Map (Map)
import qualified Data.Map as M
import Language.Mimsa.Types
import System.Random

libraryFunctions :: Map FuncName (MonoType, IO Expr)
libraryFunctions = M.singleton (FuncName "randomInt") getRandom

getRandom :: (MonoType, IO Expr)
getRandom = (mt, action)
  where
    mt = MTInt
    action = MyLiteral <$> MyInt <$> randomIO
