{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types.Typechecker where

import Data.Map (Map)
import Language.Mimsa.Types.MonoType
import Language.Mimsa.Types.Name
import Language.Mimsa.Types.Scheme

type Environment = Map Name Scheme

type Substitutions = Map Name MonoType
