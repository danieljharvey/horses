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
    (M.singleton "Wrapped" [ConsName "String" mempty])

-- | Identity monad
dtIdentity :: DataType
dtIdentity =
  DataType
    "Identity"
    ["a"]
    (M.singleton "Identity" [VarName "a"])

-- | Maybe monad
dtMaybe :: DataType
dtMaybe =
  DataType
    "Maybe"
    ["a"]
    (M.fromList [("Just", [VarName "a"]), ("Nothing", [])])

-- | Either monad
dtEither :: DataType
dtEither =
  DataType
    "Either"
    ["e", "a"]
    (M.fromList [("Right", [VarName "a"]), ("Left", [VarName "e"])])

-- | These monad
dtThese :: DataType
dtThese =
  DataType
    "These"
    ["a", "b"]
    ( M.fromList
        [ ("This", [VarName "a"]),
          ("That", [VarName "b"]),
          ("These", [VarName "a", VarName "b"])
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
            [ VarName "a",
              ConsName "List" [VarName "a"]
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
            [ VarName "a",
              VarName "b",
              ConsName "DoubleList" [VarName "a", VarName "b"]
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
        [ ("Leaf", [VarName "a"]),
          ( "Branch",
            [ ConsName "Tree" [VarName "a"],
              ConsName "Tree" [VarName "a"]
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
        [ TNFunc
            (VarName "r")
            (VarName "a")
        ]
    )

dtMatchedPair :: DataType
dtMatchedPair =
  DataType
    "MatchedPair"
    ["a"]
    (M.singleton "MatchedPair" [VarName "a", VarName "a"])

dtConsoleF :: DataType
dtConsoleF =
  DataType
    "ConsoleF"
    ["next"]
    ( M.fromList
        [ ("Write", [ConsName "String" [], VarName "next"]),
          ("Read", [TNFunc (ConsName "String" []) (VarName "next")])
        ]
    )

dtPair :: DataType
dtPair = DataType "Pair" ["a", "b"] (M.singleton "Pair" [VarName "a", VarName "b"])

typecheckInstance ::
  (DataType -> Either Text (Expr Name ())) ->
  DataType ->
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
