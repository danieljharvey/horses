{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Mimsa.Modules.Prelude (prelude, preludeHash, maybeInput, preludeInput, 
      stateInput) where

-- hard coding a Prelude in here for testing
-- this is not The Way however we have a chicken/egg situation in terms of
-- implementing imports/exports/other modules so this should unblock us
--
import Data.FileEmbed
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import Language.Mimsa.Modules.HashModule
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Modules.DefIdentifier
import Language.Mimsa.Types.Modules.Module
import Language.Mimsa.Types.Modules.ModuleHash
import Language.Mimsa.Types.Typechecker

preludeHash :: ModuleHash
preludeHash = hashModule prelude

-- this is only really used in tests, we should move it there
prelude :: Module Annotation
prelude =
  Module
    { moExpressions = exprs,
      moDataTypes = dts,
      moExpressionExports = S.singleton (DIName "fst"),
      moExpressionImports = mempty,
      moDataTypeExports = S.singleton "Either",
      moDataTypeImports = mempty,
      moNamedImports = mempty
    }
  where
    exprs =
      M.fromList
        [ ( DIName "fst",
            MyLambda
              mempty
              (Identifier mempty "pair")
              ( MyLetPattern
                  mempty
                  (PPair mempty (PVar mempty "a") (PWildcard mempty))
                  (MyVar mempty Nothing "pair")
                  (MyVar mempty Nothing "a")
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

maybeInput :: Text
maybeInput = T.decodeUtf8 $(embedFile "static/modules/Maybe.mimsa")

stateInput :: Text
stateInput = T.decodeUtf8 $(embedFile "static/modules/State.mimsa")

preludeInput :: Text
preludeInput = T.decodeUtf8 $(embedFile "static/modules/Prelude.mimsa")

