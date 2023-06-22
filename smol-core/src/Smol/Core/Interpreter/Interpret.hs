{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Smol.Core.Interpreter.Interpret (interpret) where

import Control.Monad.Identity
import Data.Foldable (toList)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import Data.Monoid
import qualified Data.Sequence as Seq
import Smol.Core.Interpreter.Convert
import Smol.Core.Interpreter.Types
import Smol.Core.Types

interpretInfix ::
  (Monad m) =>
  ann ->
  Op ->
  IExpr ann ->
  IExpr ann ->
  m (IExpr ann)
interpretInfix ann OpAdd (IPrim _ (PNat a)) (IPrim _ (PNat b)) =
  pure $ IPrim ann (PNat $ a + b)
interpretInfix ann OpAdd (IPrim _ (PInt a)) (IPrim _ (PInt b)) =
  pure $ IPrim ann (PInt $ a + b)
interpretInfix ann OpAdd (IPrim _ (PNat a)) (IPrim _ (PInt b)) =
  pure $ IPrim ann (PInt (fromIntegral a + b))
interpretInfix ann OpAdd (IPrim _ (PInt a)) (IPrim _ (PNat b)) =
  pure $ IPrim ann (PInt (a + fromIntegral b))
interpretInfix _ _ _ _ = error "haven't implemented other infixes"

-- | just keep reducing the thing until the smallest thing
interpret ::
  (
    Monad m,
    Monoid ann,
    Show ann
  ) =>
  IExpr ann ->
  m (IExpr ann)
interpret (IVar ann a) = pure (IVar ann a)
interpret (IGlobal _ _) = error "Cannot use globals in interpreter"
interpret (IPrim ann p) = pure (IPrim ann p)
interpret (IInfix ann op a b) = do
  rA <- interpret a
  rB <- interpret b
  interpretInfix ann op rA rB
interpret (IArray ann as) =
  IArray ann <$> traverse interpret as
interpret (IApp _ (ILambda _ _ fn) arg) =
  interpret arg >>= interpret . fn
interpret (IApp annA (IConstructor annB constructor) arg) =
  IApp annA (IConstructor annB constructor) <$> interpret arg
interpret (IApp ann fn arg) =
  IApp ann <$> interpret fn <*> interpret arg
interpret (ITuple ann a as) =
  ITuple ann <$> interpret a <*> traverse interpret as
interpret (IIf _ predE thenE elseE) = do
  rPred <- interpret predE
  case rPred of
    (IPrim _ (PBool True)) -> interpret thenE
    (IPrim _ (PBool False)) -> interpret elseE
    other -> error $ "not a bool predicate" <> show (toExpr other)
interpret (IConstructor ann constructor) =
  pure $ IConstructor ann constructor
interpret (IPatternMatch _ann patExpr pats) = do
  rPatExpr <- interpret patExpr
  interpretPatternMatch rPatExpr pats
interpret (IRecord ann as) =
  IRecord ann <$> traverse interpret as
interpret (IRecordAccess _ record ident) = do
  rRecord <- interpret record
  case rRecord of
    IRecord _ items -> case M.lookup ident items of
      Just found -> interpret found
      Nothing -> error $ "Could not find " <> show ident
    other -> error $ "Expected record, found " <> show (toExpr other)
interpret (ILambda ann ident body) = pure (ILambda ann ident body)
interpret (IAnn _ _ expr) = interpret expr

interpretPatternMatch ::
  ( Monoid ann,
    Monad m,
    Show ann
  ) =>
  IExpr ann ->
  NE.NonEmpty (Pattern Identity ann, IExpr ann -> IExpr ann) ->
  m (IExpr ann)
interpretPatternMatch expr' patterns = do
  -- interpret match expression
  intExpr <- interpret expr'
  let foldF (pat, patExpr) = case patternMatches pat intExpr of
        Just bindings -> First (Just (patExpr, bindings))
        _ -> First Nothing

  -- get first matching pattern
  case getFirst (foldMap foldF patterns) of
    Just (patExpr, bindings) ->
      do
        -- run body with closure + new arg
        -- need to pass in the shit
        let value = IRecord mempty (M.fromList bindings)

        interpret (patExpr value)
    _ ->
      error "pattern match failure"

-- pull vars out of expr to match patterns
patternMatches ::
  Pattern Identity ann ->
  IExpr ann ->
  Maybe [(Identifier, IExpr ann)]
patternMatches (PWildcard _) _ = pure []
patternMatches (PVar _ name) expr = pure [(runIdentity name, expr)]
{-
patternMatches (PPair _ pA pB) (MyPair _ a b) = do
  as <- patternMatches pA a
  bs <- patternMatches pB b
  pure $ as <> bs
patternMatches (PRecord _ pAs) (MyRecord _ as)
  | S.null (S.difference (M.keysSet pAs) (M.keysSet as)) = do
    let usefulInputs = M.intersection as pAs
        allPairs = zip (M.elems pAs) (M.elems usefulInputs)
    nice <- traverse (uncurry patternMatches) allPairs
    pure (mconcat nice)
-}
patternMatches (PLiteral _ pB) (IPrim _ b)
  | pB == b = pure mempty
patternMatches (PConstructor _ _pTyCon []) (IConstructor _ _tyCon) =
  pure mempty
patternMatches (PConstructor _ pTyCon pArgs) (IApp ann fn val) = do
  (tyCon, args) <- consAppToPattern (IApp ann fn val)
  if tyCon /= runIdentity pTyCon
    then Nothing
    else do
      let allPairs = zip pArgs args
      nice <- traverse (uncurry patternMatches) allPairs
      pure (mconcat nice)
patternMatches (PArray _ pAs NoSpread) (IArray _ as)
  | length pAs == length as = do
      let allPairs = zip pAs (toList as)
      nice <- traverse (uncurry patternMatches) allPairs
      pure (mconcat nice)
patternMatches (PArray _ pAs (SpreadWildcard _)) (IArray _ as)
  | length pAs <= length as = do
      let allPairs = zip pAs (toList as)
      nice <- traverse (uncurry patternMatches) allPairs
      pure (mconcat nice)
patternMatches (PArray _ pAs (SpreadValue _ a)) (IArray ann as)
  | length pAs <= length as = do
      let binding = (runIdentity a, IArray ann (Seq.fromList $ drop (length pAs) (toList as)))
          allPairs = zip pAs (toList as)
      nice <- traverse (uncurry patternMatches) allPairs
      pure (mconcat nice <> [binding])
{-
patternMatches (PString _ pA pAs) (MyLiteral _ (MyString (StringType str))) | not (T.null str) =
  do
    let bindingA = case pA of
          (StrValue ann a) ->
            [ ( a,
                MyLiteral
                  ann
                  ( MyString
                      ( StringType (T.singleton (T.head str))
                      )
                  )
              )
            ]
          _ -> []
        bindingAs = case pAs of
          (StrValue ann as) ->
            [ ( as,
                MyLiteral
                  ann
                  ( MyString (StringType (T.drop 1 str))
                  )
              )
            ]
          _ -> []
    pure (bindingA <> bindingAs)
-}
patternMatches _ _ = Nothing

consAppToPattern :: IExpr ann -> Maybe (Constructor, [IExpr ann])
consAppToPattern (IApp _ fn val) = do
  (tyCon, more) <- consAppToPattern fn
  pure (tyCon, more <> [val])
consAppToPattern (IConstructor _ tyCon) = pure (tyCon, mempty)
consAppToPattern _ = Nothing
