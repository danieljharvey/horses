{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Modules.Prelude (prelude, preludeHash) where

-- hard coding a Prelude in here for testing
-- this is not The Way however we have a chicken/egg situation in terms of
-- implementing imports/exports/other modules so this should unblock us
--

import qualified Data.Map as M
import qualified Data.Set as S
import Language.Mimsa.Modules.HashModule
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Modules.Module
import Language.Mimsa.Types.Modules.ModuleHash
import Language.Mimsa.Types.Typechecker

preludeHash :: ModuleHash
preludeHash = hashModule prelude

prelude :: Module Annotation
prelude =
  Module
    { moExpressions = exprs,
      moDataTypes = dts,
      moExpressionExports = S.singleton "fst",
      moExpressionImports = mempty,
      moDataTypeExports = S.singleton "Either",
      moDataTypeImports = mempty
    }
  where
    exprs =
      M.fromList
        [ ( "fst",
            MyLambda
              mempty
              (Identifier mempty "pair")
              ( MyLetPattern
                  mempty
                  (PPair mempty (PVar mempty "a") (PWildcard mempty))
                  (MyVar mempty "pair")
                  (MyVar mempty "a")
              )
          )
        ]
    dts =
      M.fromList
        [ ( "Either",
            DataType
              "Either"
              ["e", "a"]
              ( M.fromList
                  [ ("Left", [MTVar mempty (TVName "e")]),
                    ("Right", [MTVar mempty (TVName "a")])
                  ]
              )
          )
        ]
