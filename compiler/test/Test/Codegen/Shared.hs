{-# LANGUAGE OverloadedStrings #-}

module Test.Codegen.Shared
  ( typecheckInstance,
    unsafeParse,
    dtVoid,
    dtTrafficLights,
    dtWrappedString,
    dtIdentity,
    dtMaybe,
    dtEither,
    dtPair,
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

import Data.Bifunctor (first)
import Data.Functor
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Language.Mimsa.Actions.Shared
import Language.Mimsa.Parser
import Language.Mimsa.Printer
import Language.Mimsa.Typechecker.DataTypes
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.ResolvedExpression
import Language.Mimsa.Types.Typechecker.MonoType
import Test.Data.Project

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
    (M.singleton "Wrapped" [dataTypeWithVars mempty "String" mempty])

-- | Identity monad
dtIdentity :: DataType
dtIdentity =
  DataType
    "Identity"
    ["a"]
    (M.singleton "Identity" [MTVar mempty (TVName "a")])

-- | Maybe monad
dtMaybe :: DataType
dtMaybe =
  DataType
    "Maybe"
    ["a"]
    ( M.fromList
        [ ("Just", [MTVar mempty (TVName "a")]),
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
        [ ("Right", [MTVar mempty (TVName "a")]),
          ("Left", [MTVar mempty (TVName "e")])
        ]
    )

-- | These monad
dtThese :: DataType
dtThese =
  DataType
    "These"
    ["a", "b"]
    ( M.fromList
        [ ("This", [MTVar mempty (TVName "a")]),
          ("That", [MTVar mempty (TVName "b")]),
          ( "These",
            [ MTVar mempty (TVName "a"),
              MTVar
                mempty
                (TVName "b")
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
            [ MTVar mempty (TVName "a"),
              dataTypeWithVars mempty "List" [MTVar mempty (TVName "a")]
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
            [ MTVar mempty (TVName "a"),
              MTVar mempty (TVName "b"),
              dataTypeWithVars
                mempty
                "DoubleList"
                [ MTVar mempty (TVName "a"),
                  MTVar mempty (TVName "b")
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
        [ ("Leaf", [MTVar mempty (TVName "a")]),
          ( "Branch",
            [ dataTypeWithVars mempty "Tree" [MTVar mempty (TVName "a")],
              dataTypeWithVars mempty "Tree" [MTVar mempty (TVName "a")]
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
            (MTVar mempty (TVName "r"))
            (MTVar mempty (TVName "a"))
        ]
    )

dtMatchedPair :: DataType
dtMatchedPair =
  DataType
    "MatchedPair"
    ["a"]
    ( M.singleton
        "MatchedPair"
        [ MTVar mempty (TVName "a"),
          MTVar mempty (TVName "a")
        ]
    )

dtConsoleF :: DataType
dtConsoleF =
  DataType
    "ConsoleF"
    ["next"]
    ( M.fromList
        [ ( "Write",
            [ dataTypeWithVars mempty "String" [],
              MTVar mempty (TVName "next")
            ]
          ),
          ( "Read",
            [ MTFunction
                mempty
                (dataTypeWithVars mempty "String" [])
                (MTVar mempty (TVName "next"))
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
        [ MTVar mempty (TVName "a"),
          MTVar mempty (TVName "b")
        ]
    )

dtEnv :: DataType
dtEnv =
  DataType
    "Env"
    ["w", "a"]
    ( M.singleton
        "Env"
        [ MTVar mempty (TVName "w"),
          MTVar mempty (TVName "a")
        ]
    )

typecheckInstance ::
  (DataType -> Either Text (Expr Name ())) ->
  DataType ->
  Either (Error Annotation) (ResolvedExpression Annotation)
typecheckInstance mkInstance dt =
  (,) <$> newStdLib <*> inst'
    >>= ( \(testStdlib', expr) ->
            getTypecheckedStoreExpression (prettyPrint expr) testStdlib' expr
        )
  where
    newStdLib =
      addExprBinding
        ( MyData
            mempty
            dt
            (MyRecord mempty mempty)
        )
        "temporaryAddType"
        testStdlib
    inst' =
      first ParseError (fmap ($> mempty) (mkInstance dt))

unsafeParse :: Text -> Expr Name ()
unsafeParse t = case parseExprAndFormatError t of
  Right a -> a $> mempty
  Left e -> error (T.unpack e)
