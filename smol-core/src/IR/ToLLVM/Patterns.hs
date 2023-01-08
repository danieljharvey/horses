{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module IR.ToLLVM.Patterns
  ( createSelectTable,
    predicatesToOperand,
    patName,
    selectToOperand,
    GetPath (..),
    PatternPredicate (..),
  )
where

import Control.Monad.Except
import qualified Data.ByteString.Short as SBS
import qualified Data.List.NonEmpty as NE
import Data.String
import IR.IRExpr
import IR.ToLLVM.Helpers
import qualified LLVM.AST.IntegerPredicate as IP
import qualified LLVM.AST.Operand as Op
import qualified LLVM.IRBuilder.Constant as C
import qualified LLVM.IRBuilder.Instruction as L
import qualified LLVM.IRBuilder.Module as L
import qualified LLVM.IRBuilder.Monad as L
import Types.GetPath
import Types.PatternPredicate

-- | get name for pattern block
patName :: Integer -> SBS.ShortByteString
patName i = "pat" <> fromString (show i)

-- given a big predicate, make it into one operand
predicatesToOperand ::
  (L.MonadModuleBuilder m, L.MonadIRBuilder m) =>
  Op.Operand ->
  NE.NonEmpty (PatternPredicate IRPrim) ->
  m Op.Operand
predicatesToOperand input nePreds = do
  firstOp <- compilePred (NE.head nePreds)
  foldM
    ( \op pat -> do
        predOp <- compilePred pat
        L.and op predOp
    )
    firstOp
    (NE.tail nePreds)
  where
    compilePred (PathEquals (StructPath as) prim) =
      do
        val <- loadFromStruct input (NE.toList as)
        L.icmp IP.EQ val (irPrimToLLVM prim)
    compilePred (PathEquals ValuePath prim) =
      L.icmp IP.EQ input (irPrimToLLVM prim)

-- | captures the idea of "if this predicate then 0, if this predicate then
-- 1..."
-- etc
createSelectTable ::
  NE.NonEmpty a ->
  SelectList a
createSelectTable =
  withPats 0
  where
    withPats i pats = case NE.uncons pats of
      (_, Nothing) -> SelectThen i
      (pat, Just morePats) ->
        SelectOr i pat $ withPats (i + 1) morePats

-- numbered list
data SelectList a
  = SelectOr Integer a (SelectList a)
  | SelectThen Integer

-- | combines the select into one operand
-- need to work out how to bitcast after peeking at the first value
-- so to make sure any further items we peek at are correct
selectToOperand ::
  ( L.MonadModuleBuilder m,
    L.MonadIRBuilder m
  ) =>
  Op.Operand ->
  SelectList ([PatternPredicate IRPrim], IRType) ->
  m Op.Operand
selectToOperand input = go
  where
    go (SelectThen i) = pure (C.int32 i) `L.named` "fallback"
    go (SelectOr i (preds, irType) rest) = do
      opRest <- go rest
      opPred <- case NE.nonEmpty preds of
        Just nePreds -> do
          castInput <- L.bitcast input (irTypeToLLVM (getCastType irType))
          predicatesToOperand castInput nePreds `L.named` "pred"
        Nothing -> pure (C.bit 1) -- ie, const True
      L.select opPred (C.int32 i) opRest
