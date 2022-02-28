{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Language.Mimsa.Store.ExprGraph
  ( createExprGraph,
    numberExpr,
  )
where

import Control.Monad.State
import Control.Monad.Writer
import Data.Text (Text)
import Language.Mimsa.Printer
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Graphviz
import Language.Mimsa.Types.Identifiers

createExprGraph :: Expr Name ann -> [Graphviz Int Text]
createExprGraph expr = snd $ runWriter (createGraph (snd <$> numberExpr expr))
  where
    createGraph :: Expr Name Int -> Writer [Graphviz Int Text] Int
    createGraph (MyLiteral ann val) = do
      tell [Node ann (prettyPrint val)]
      pure ann
    createGraph (MyIf ann predExpr thenExpr elseExpr) = do
      predId <- createGraph predExpr
      thenId <- createGraph thenExpr
      elseId <- createGraph elseExpr
      tell
        [ Node ann "if",
          Edge ann predId (Just "predicate"),
          Edge ann thenId (Just "then"),
          Edge ann elseId (Just "else")
        ]
      pure ann
    createGraph (MyLambda ann ident expr') = do
      exprId <- createGraph expr'
      tell
        [ Node ann ("fn " <> prettyPrint ident),
          Edge ann exprId Nothing
        ]
      pure ann
    createGraph (MyPatternMatch ann matchExpr patterns) = do
      matchId <- createGraph matchExpr
      labelledPatterns <-
        traverse
          ( \(pat, patExpr) ->
              (,)
                (prettyPrint pat)
                <$> createGraph patExpr
          )
          patterns
      tell
        [ Node ann "match",
          Edge ann matchId Nothing
        ]
      tell
        ( ( \(lbl, edgeId) ->
              Edge ann edgeId (Just lbl)
          )
            <$> labelledPatterns
        )
      pure ann
    createGraph (MyVar ann var) = do
      tell [Node ann (prettyPrint var)]
      pure ann
    createGraph _ = error "bum"

getNum :: (MonadState Int m) => m Int
getNum = do
  num <- get
  put (num + 1)
  pure num

numberExprState ::
  (MonadState Int m) =>
  Expr var ann ->
  m (Expr var (ann, Int))
numberExprState expr =
  let baseExpr = (,0 :: Int) <$> expr
      f (ann, _) = (,) ann <$> getNum
   in traverse f baseExpr

numberExpr ::
  Expr var ann ->
  Expr var (ann, Int)
numberExpr expr = evalState (numberExprState expr) 0
