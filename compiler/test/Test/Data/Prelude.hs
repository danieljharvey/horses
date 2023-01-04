{-# LANGUAGE OverloadedStrings #-}

module Test.Data.Prelude (prelude, preludeHash) where

-- hard coded basic Prelude module used for tests

import Data.Functor
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Language.Mimsa.Core
import Language.Mimsa.Modules.HashModule
import Test.Utils.Helpers

preludeHash :: ModuleHash
preludeHash = snd (serializeModule prelude)

prelude :: Module Annotation
prelude =
  Module
    { moExpressions = exprs,
      moDataTypes = dts,
      moExpressionExports = S.fromList [DIName "fst", DIInfix (InfixOp "<|>")],
      moExpressionImports = mempty,
      moDataTypeExports = S.singleton "Either",
      moDataTypeImports = mempty,
      moNamedImports = mempty
    }
  where
    exprs =
      M.fromList
        [ ( DIName "fst",
            unsafeParseExpr "\\pair -> let (a,_) = pair in a" $> mempty
          ),
          ( DIInfix (InfixOp "<|>"),
            unsafeParseExpr "\\eA -> \\eB -> match eA with Right a -> Right a | Left e -> eB" $> mempty
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
