{-# LANGUAGE OverloadedStrings #-}

module Test.Typechecker.Codegen.Shared
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
import Language.Mimsa.Actions
import Language.Mimsa.Parser
import Language.Mimsa.Printer
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.ResolvedExpression
import Language.Mimsa.Types.Typechecker.MonoType
import Test.Data.Project

-- | has no constructors, we can do nothing with this
dtVoid :: DataType ann
dtVoid = DataType "Void" mempty mempty

-- | an enum, we can go to and from a string
dtTrafficLights :: DataType ann
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
dtWrappedString :: (Monoid ann) => DataType ann
dtWrappedString =
  DataType
    "WrappedString"
    mempty
    (M.singleton "Wrapped" [MTData mempty "String" mempty])

-- | Identity monad
dtIdentity :: (Monoid ann) => DataType ann
dtIdentity =
  DataType
    "Identity"
    ["a"]
    (M.singleton "Identity" [MTVar mempty (TVName "a")])

-- | Maybe monad
dtMaybe :: (Monoid ann) => DataType ann
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
dtEither :: (Monoid ann) => DataType ann
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
dtThese :: (Monoid ann) => DataType ann
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
dtList :: (Monoid ann) => DataType ann
dtList =
  DataType
    "List"
    ["a"]
    ( M.fromList
        [ ( "Cons",
            [ MTVar mempty (TVName "a"),
              MTData mempty "List" [MTVar mempty (TVName "a")]
            ]
          ),
          ("Nil", [])
        ]
    )

-- | List but with more type params so we can recurse around more complicated
-- types
dtDoubleList :: (Monoid ann) => DataType ann
dtDoubleList =
  DataType
    "DoubleList"
    ["a", "b"]
    ( M.fromList
        [ ( "DoubleCons",
            [ MTVar mempty (TVName "a"),
              MTVar mempty (TVName "b"),
              MTData
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

dtTree :: (Monoid ann) => DataType ann
dtTree =
  DataType
    "Tree"
    ["a"]
    ( M.fromList
        [ ("Leaf", [MTVar mempty (TVName "a")]),
          ( "Branch",
            [ MTData mempty "Tree" [MTVar mempty (TVName "a")],
              MTData mempty "Tree" [MTVar mempty (TVName "a")]
            ]
          )
        ]
    )

dtReader :: (Monoid ann) => DataType ann
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

dtMatchedPair :: (Monoid ann) => DataType ann
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

dtConsoleF :: (Monoid ann) => DataType ann
dtConsoleF =
  DataType
    "ConsoleF"
    ["next"]
    ( M.fromList
        [ ( "Write",
            [ MTData mempty "String" [],
              MTVar mempty (TVName "next")
            ]
          ),
          ( "Read",
            [ MTFunction
                mempty
                (MTData mempty "String" [])
                (MTVar mempty (TVName "next"))
            ]
          )
        ]
    )

dtPair :: (Monoid ann) => DataType ann
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

dtEnv :: (Monoid ann) => DataType ann
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
  (DataType () -> Either Text (Expr Name ())) ->
  DataType () ->
  Either (Error Annotation) (ResolvedExpression Annotation)
typecheckInstance mkInstance dt =
  (,) <$> newStdLib <*> inst'
    >>= ( \(stdLib', expr) ->
            getTypecheckedStoreExpression (prettyPrint expr) stdLib' expr
        )
  where
    newStdLib = addBinding (prettyPrint dt <> " in {}") "temporaryAddType" stdLib
    inst' =
      first ParseError (fmap ($> mempty) (mkInstance dt))

unsafeParse :: Text -> Expr Name ()
unsafeParse t = case parseExprAndFormatError t of
  Right a -> a $> mempty
  Left e -> error (T.unpack e)
