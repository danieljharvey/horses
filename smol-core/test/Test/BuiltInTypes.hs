{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Test.BuiltInTypes
  ( builtInTypes,
  )
where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Smol.Core.Types

-- | these should move to the test suite, and instead we should rely on types
-- defined in module
builtInTypes ::
  (Monoid ann, Ord (dep TypeName)) =>
  (forall a. a -> dep a) ->
  Map (dep TypeName) (DataType dep ann)
builtInTypes liftDep =
  let identityDt =
        DataType
          "Identity"
          ["a"]
          (M.singleton "Identity" [TVar mempty $ liftDep "a"])
      maybeDt =
        DataType
          "Maybe"
          ["a"]
          (M.fromList [("Just", [TVar mempty $ liftDep "a"]), ("Nothing", [])])
      eitherDt =
        DataType
          "Either"
          ["e", "a"]
          ( M.fromList
              [ ("Left", [TVar mempty $ liftDep "e"]),
                ("Right", [TVar mempty $ liftDep "a"])
              ]
          )

      theseDt =
        DataType
          "These"
          ["a", "b"]
          ( M.fromList
              [ ("This", [TVar mempty $ liftDep "a"]),
                ("That", [TVar mempty $ liftDep "b"]),
                ("These", [TVar mempty $ liftDep "a", TVar mempty $ liftDep "b"])
              ]
          )
      ordDt =
        DataType
          "Ord"
          []
          ( M.fromList [("LT", mempty), ("EQ", mempty), ("GT", mempty)]
          )
      listDt =
        DataType
          "List"
          ["a"]
          ( M.fromList
              [ ( "Cons",
                  [ TVar mempty (liftDep "a"),
                    TApp mempty (TConstructor mempty (liftDep "List")) (TVar mempty (liftDep "a"))
                  ]
                ),
                ("Nil", mempty)
              ]
          )
   in M.fromList
        [ (liftDep "Maybe", maybeDt),
          (liftDep "Either", eitherDt),
          (liftDep "Ord", ordDt),
          (liftDep "These", theseDt),
          (liftDep "Identity", identityDt),
          (liftDep "List", listDt)
        ]
