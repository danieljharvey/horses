{-# LANGUAGE OverloadedStrings #-}

module Test.Data.Prelude (prelude, preludeHash) where

-- hard coded basic Prelude module used for tests

import Data.Functor
import qualified Data.Map as M
import qualified Data.Set as S
import Language.Mimsa.Modules.HashModule
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Modules.DefIdentifier
import Language.Mimsa.Types.Modules.Module
import Language.Mimsa.Types.Modules.ModuleHash
import Language.Mimsa.Types.Typechecker
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
