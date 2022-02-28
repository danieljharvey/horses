{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types.Graphviz
  ( Graphviz (..),
    prettyGraphviz,
  )
where

import qualified Data.Set as S
import Data.Text (Text)
import Language.Mimsa.Printer

----

data Graphviz k v = Edge k k (Maybe v) | Node k v
  deriving stock (Eq, Ord, Show)

instance (Ord k, Printer k, Printer v) => Printer (Graphviz k v) where
  prettyPrint (Edge from to label) =
    showHash from <> " -> " <> showHash to <> showLabel
    where
      showHash hash = "\"" <> prettyPrint hash <> "\""
      showLabel = case label of
        Just lbl -> " [ label = \"" <> prettyPrint lbl <> "\" ]"
        Nothing -> ""
  prettyPrint (Node hash name) =
    "\"" <> prettyPrint hash <> "\" [ label = \"" <> prettyPrint name <> "\" ]"

prettyGraphviz ::
  (Ord k, Ord v, Printer k, Printer v) =>
  [Graphviz k v] ->
  Text
prettyGraphviz gv =
  let parts = (\a -> "  " <> a <> "\n") . prettyPrint <$> S.toList (S.fromList gv)
   in "strict digraph {\n" <> mconcat parts <> "}"
