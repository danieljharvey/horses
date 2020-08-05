{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Store.DepGraph where

import Data.Map ((!))
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Language.Mimsa.Store.Storage
import Language.Mimsa.Types

-----

newtype DepInfo = DepInfo (Name, ExprHash)
  deriving (Eq, Ord, Show)

instance Printer DepInfo where
  prettyPrint (DepInfo (name, exprHash)) =
    "+ " <> prettyPrint name <> "     (" <> prettyPrint exprHash <> ")"

-----

data DepGraph
  = Func DepInfo [DepGraph]
  deriving (Eq, Ord, Show)

instance Printer DepGraph where
  prettyPrint (Func info children) = showFuncLine 0 info children

-----

showFuncLine :: Int -> DepInfo -> [DepGraph] -> Text
showFuncLine i info children = spaces <> prettyPrint info <> children'
  where
    spaces = T.pack (replicate i ' ')
    children' = case children of
      [] -> ""
      (a : as) ->
        "\n"
          <> T.intercalate
            "\n"
            ( ( \(Func subName subChildren) ->
                  showFuncLine (i + 2) subName subChildren
              )
                <$> (a : as)
            )

createDepGraph :: Name -> Store -> StoreExpression -> DepGraph
createDepGraph name (Store store') storeExpr' = Func depInfo leaves
  where
    depInfo = DepInfo (name, getStoreExpressionHash storeExpr')
    leaves =
      ( \(name', hash) ->
          createDepGraph name' (Store store') (store' ! hash)
      )
        <$> children
    children = M.toList . getBindings . storeBindings $ storeExpr'
