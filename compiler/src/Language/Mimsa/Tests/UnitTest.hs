{-# LANGUAGE FlexibleContexts #-}

module Language.Mimsa.Tests.UnitTest
  ( resultIsBoolean,
  )
where

import Language.Mimsa.Tests.Helpers
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Typechecker

resultIsBoolean :: MonoType -> Either TypeError ()
resultIsBoolean mt = do
  unifies
    mt
    (MTPrim mempty MTBool)
