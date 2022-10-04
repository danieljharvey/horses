{-# LANGUAGE OverloadedStrings #-}

module Test.Codegen.Shared
  ( unsafeParse,
    dtVoid,
    dtTrafficLights,
    dtWrappedString,
    dtIdentity,
    dtMaybe,
    dtEither,
    dtPair,
    dtMonoPair,
    dtThese,
    dtList,
    dtDoubleList,
    dtTree,
    dtReader,
    dtMatchedPair,
    dtConsoleF,
    dtEnv,
  )
where

import Data.Functor
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Language.Mimsa.Parser
import Language.Mimsa.Typechecker.DataTypes
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Typechecker.MonoType
import Test.Utils.Helpers

-- | has no constructors, we can do nothing with this
dtVoid :: DataType
dtVoid = DataType "Void" mempty mempty

-- | an enum, we can go to and from a string
dtTrafficLights :: DataType
dtTrafficLights =
  DataType
    "TrafficLights"
    mempty
    ( M.fromList
        [ ("Red", mempty),
          ("Yellow", mempty),
          ("Green", mempty)
        ]
    )

-- | A newtype around a string
-- | we can wrap and unwrap maybe?
dtWrappedString :: DataType
dtWrappedString =
  DataType
    "WrappedString"
    mempty
    (M.singleton "Wrapped" [dataTypeWithVars mempty Nothing "String" mempty])

-- | Identity monad
dtIdentity :: DataType
dtIdentity =
  DataType
    "Identity"
    ["a"]
    (M.singleton "Identity" [MTVar mempty (tvNamed "a")])

-- | Maybe monad
dtMaybe :: DataType
dtMaybe =
  DataType
    "Maybe"
    ["a"]
    ( M.fromList
        [ ("Just", [MTVar mempty (tvNamed "a")]),
          ("Nothing", [])
        ]
    )

-- | Either monad
dtEither :: DataType
dtEither =
  DataType
    "Either"
    ["e", "a"]
    ( M.fromList
        [ ("Right", [MTVar mempty (tvNamed "a")]),
          ("Left", [MTVar mempty (tvNamed "e")])
        ]
    )

-- | These monad
dtThese :: DataType
dtThese =
  DataType
    "These"
    ["a", "b"]
    ( M.fromList
        [ ("This", [MTVar mempty (tvNamed "a")]),
          ("That", [MTVar mempty (tvNamed "b")]),
          ( "These",
            [ MTVar mempty (tvNamed "a"),
              MTVar
                mempty
                (tvNamed "b")
            ]
          )
        ]
    )

-- | List monad
dtList :: DataType
dtList =
  DataType
    "List"
    ["a"]
    ( M.fromList
        [ ( "Cons",
            [ MTVar mempty (tvNamed "a"),
              dataTypeWithVars mempty Nothing "List" [MTVar mempty (tvNamed "a")]
            ]
          ),
          ("Nil", [])
        ]
    )

-- | List but with more type params so we can recurse around more complicated
-- types
dtDoubleList :: DataType
dtDoubleList =
  DataType
    "DoubleList"
    ["a", "b"]
    ( M.fromList
        [ ( "DoubleCons",
            [ MTVar mempty (tvNamed "a"),
              MTVar mempty (tvNamed "b"),
              dataTypeWithVars
                mempty
                Nothing
                "DoubleList"
                [ MTVar mempty (tvNamed "a"),
                  MTVar mempty (tvNamed "b")
                ]
            ]
          ),
          ("DoubleNil", [])
        ]
    )

dtTree :: DataType
dtTree =
  DataType
    "Tree"
    ["a"]
    ( M.fromList
        [ ("Leaf", [MTVar mempty (tvNamed "a")]),
          ( "Branch",
            [ dataTypeWithVars mempty Nothing "Tree" [MTVar mempty (tvNamed "a")],
              dataTypeWithVars mempty Nothing "Tree" [MTVar mempty (tvNamed "a")]
            ]
          )
        ]
    )

dtReader :: DataType
dtReader =
  DataType
    "Reader"
    ["r", "a"]
    ( M.singleton
        "Reader"
        [ MTFunction
            mempty
            (MTVar mempty (tvNamed "r"))
            (MTVar mempty (tvNamed "a"))
        ]
    )

dtMatchedPair :: DataType
dtMatchedPair =
  DataType
    "MatchedPair"
    ["a"]
    ( M.singleton
        "MatchedPair"
        [ MTVar mempty (tvNamed "a"),
          MTVar mempty (tvNamed "a")
        ]
    )

dtConsoleF :: DataType
dtConsoleF =
  DataType
    "ConsoleF"
    ["next"]
    ( M.fromList
        [ ( "Write",
            [ dataTypeWithVars mempty Nothing "String" [],
              MTVar mempty (tvNamed "next")
            ]
          ),
          ( "Read",
            [ MTFunction
                mempty
                (dataTypeWithVars mempty Nothing "String" [])
                (MTVar mempty (tvNamed "next"))
            ]
          )
        ]
    )

dtPair :: DataType
dtPair =
  DataType
    "Pair"
    ["a", "b"]
    ( M.singleton
        "Pair"
        [ MTVar mempty (tvNamed "a"),
          MTVar mempty (tvNamed "b")
        ]
    )

dtMonoPair :: DataType
dtMonoPair =
  DataType
    "MonoPair"
    ["a"]
    ( M.singleton
        "MonoPair"
        [ MTVar mempty (tvNamed "a"),
          MTVar mempty (tvNamed "a")
        ]
    )

dtEnv :: DataType
dtEnv =
  DataType
    "Env"
    ["w", "a"]
    ( M.singleton
        "Env"
        [ MTVar mempty (tvNamed "w"),
          MTVar mempty (tvNamed "a")
        ]
    )

unsafeParse :: Text -> Expr Name ()
unsafeParse t = case parseExprAndFormatError t of
  Right a -> a $> mempty
  Left e -> error (T.unpack e)
