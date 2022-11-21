{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Typechecker.Exhaustiveness
  ( isExhaustive,
    redundantCases,
    validatePatterns,
    noDuplicateVariables,
    smallerListVersions,
  )
where

import Language.Mimsa.Logging 
import Control.Monad.Except
import Data.Foldable
import Data.Functor
import Data.List (nub)
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Monoid
import qualified Data.Set as S
import Language.Mimsa.Printer
import Language.Mimsa.Typechecker.Environment
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Typechecker.Environment

validatePatterns ::
  ( MonadError (TypeErrorF var Annotation) m,
    Ord var,
    Printer var,
    Printer (Pattern var Annotation),

    Show var
  ) =>
  Environment ->
  Annotation ->
  [Pattern var Annotation] ->
  m ()
validatePatterns env ann patterns = do
  traverse_ noDuplicateVariables patterns
  missing <- isExhaustive env patterns
  _ <- case missing of
    [] -> pure ()
    _ ->
      throwError (PatternMatchErr (MissingPatterns ann missing))
  redundant <- redundantCases env patterns
  case redundant of
    [] -> pure ()
    _ ->
      throwError (PatternMatchErr (RedundantPatterns ann redundant))

noDuplicateVariables ::
  ( MonadError (TypeErrorF var Annotation) m,
    Ord var
  ) =>
  Pattern var Annotation ->
  m ()
noDuplicateVariables pat = do
  let dupes =
        M.keysSet
          . M.filter (> 1)
          . getVariables
          $ pat
   in if S.null dupes
        then pure ()
        else
          throwError
            ( PatternMatchErr
                ( DuplicateVariableUse
                    (getPatternAnnotation pat)
                    dupes
                )
            )

getVariables ::
  (Ord var) =>
  Pattern var Annotation ->
  Map var Int
getVariables (PWildcard _) = mempty
getVariables (PLit _ _) = mempty
getVariables (PVar _ a) = M.singleton a 1
getVariables (PTuple _ a as) =
  M.unionWith (+) (getVariables a) (foldMap getVariables as)
getVariables (PRecord _ as) =
  foldr (M.unionWith (+)) mempty (getVariables <$> as)
getVariables (PArray _ as spread) =
  let vars = [getSpreadVariables spread] <> (getVariables <$> as)
   in foldr (M.unionWith (+)) mempty vars
getVariables (PConstructor _ _ _ args) =
  foldr (M.unionWith (+)) mempty (getVariables <$> args)
getVariables (PString _ a as) =
  M.unionWith (+) (getStringPartVariables a) (getStringPartVariables as)

getSpreadVariables :: (Ord var) => Spread var Annotation -> Map var Int
getSpreadVariables (SpreadValue _ a) = M.singleton a 1
getSpreadVariables _ = mempty

getStringPartVariables :: (Ord var) => StringPart var Annotation -> Map var Int
getStringPartVariables (StrWildcard _) = mempty
getStringPartVariables (StrValue _ a) = M.singleton a 1

-- | given a list of patterns, return a list of missing patterns
isExhaustive ::
  ( Eq var,
    MonadError (TypeErrorF var Annotation) m,
    Printer var,
    Printer (Pattern var Annotation),
    Show var
  ) =>
  Environment ->
  [Pattern var Annotation] ->
  m [Pattern var Annotation]
isExhaustive env patterns = do
  generated <-
    mconcat
      <$> traverse (generate env) patterns
  pure $ debugPretty "results" $ filterMissing (debugPretty "patterns" patterns) (debugPretty "generated" generated)

generate ::
  ( MonadError (TypeErrorF var Annotation) m,
    Printer var,
    Printer (Pattern var Annotation),

    Show var
  ) =>
  Environment ->
  Pattern var Annotation ->
  m [Pattern var Annotation]
generate env pat = (<>) [pat] <$> generateRequired env pat

-- | Given a pattern, generate others required for it
generateRequired ::
  ( MonadError (TypeErrorF var Annotation) m,

    Printer (Pattern var Annotation),

    Printer var,
    Show var
  ) =>
  Environment ->
  Pattern var Annotation ->
  m [Pattern var Annotation]
generateRequired _ (PLit _ (MyBool True)) =
  pure [PLit mempty (MyBool False)]
generateRequired _ (PLit _ (MyBool False)) =
  pure [PLit mempty (MyBool True)]
generateRequired _ (PLit _ (MyInt _)) =
  pure [PWildcard mempty]
generateRequired _ (PLit _ (MyString "")) =
  pure [PString mempty (StrWildcard mempty) (StrWildcard mempty)]
generateRequired _ (PLit _ (MyString _)) =
  pure [PWildcard mempty]
generateRequired env (PTuple _ a as) = do
  -- the thing that sucks here is that you don't want to annihilate
  -- unnecessarily, but also you don't want to create too many extra lads
  let genOrOriginal pat = do
                  generated <- generateRequired env (debugPretty "generated from " pat)
                  case debugPretty "generated yes" generated of
                    [] -> if isComplete pat then 
                                              pure [] else 
                                                pure [pat]
                    items -> pure items
  genAs <- traverse genOrOriginal (NE.cons a as)
  let tuple ne = PTuple mempty (NE.head ne) (NE.fromList $ NE.tail ne)
  pure (tuple <$> sequence (debugPretty "genAs" genAs))
generateRequired env (PRecord _ items) = do
  items' <- traverse (generateRequired env) items
  pure (PRecord mempty <$> sequence items')
generateRequired env (PConstructor ann modName tyCon args) = do
  dt <- lookupConstructor env ann modName tyCon
  newFromArgs <- traverse (generateRequired env) args
  newDataTypes <- requiredFromDataType dt
  let newCons = PConstructor mempty modName tyCon <$> sequence newFromArgs
  pure (newCons <> newDataTypes)
generateRequired env (PArray _ items _) = do
  items' <- traverse (generateRequired env) items
  let allItems = smallerListVersions (sequence items')
  pure $
    (PArray mempty <$> allItems <*> pure (SpreadWildcard mempty))
      <> [PArray mempty mempty NoSpread]
      <> [PArray mempty [PWildcard mempty] (SpreadWildcard mempty)]
generateRequired _ PString {} = pure [PLit mempty (MyString "")]
generateRequired _ _ = pure mempty

-- given a list [[1,2,3]], return [[1,2,3], [1,2], [1]]
smallerListVersions :: [[a]] -> [[a]]
smallerListVersions aas =
  let get x = case x of
        [] -> []
        (_ : as) -> get as <> [x]
   in get =<< aas

requiredFromDataType ::
  (MonadError (TypeErrorF var Annotation) m) =>
  DataType ->
  m [Pattern var Annotation]
requiredFromDataType (DataType _ _ cons) =
  if length cons < 2 -- if there is only one constructor don't generate more
    then pure mempty
    else do
      let new (n, as) =
            [ PConstructor
                mempty
                Nothing
                n
                (PWildcard mempty <$ as)
            ]
      pure $ mconcat (new <$> M.toList cons)

-- filter outstanding items
filterMissing ::
  (Eq var, Eq ann, Show var) =>
  [Pattern var ann] ->
  [Pattern var ann] ->
  [Pattern var ann]
filterMissing patterns required =
  nub $ foldr annihiliatePattern required patterns
  where
    annihiliatePattern pat =
      filter
        ( not
            . annihilate
              (removeAnn pat)
            . removeAnn
        )

removeAnn :: Pattern var ann -> Pattern var ()
removeAnn p = p $> ()

-- does left pattern satisfy right pattern?
annihilateAll ::
  (Eq var, Show var) =>
  [(Pattern var (), Pattern var ())] ->
  Bool
annihilateAll =
  foldr
    (\(a, b) keep -> keep && annihilate a b)
    True

-- | if left is on the right, should we get rid?
annihilate :: (Eq var, Show var) => Pattern var () -> Pattern var () -> Bool
annihilate a b | a == b = True
annihilate (PWildcard _) _ = True -- wildcard trumps all
annihilate (PVar _ _) _ = True -- as does var
annihilate (PTuple _ a as) (PTuple _ b bs) =
  let allPairs = zip ([a] <> NE.toList as) ([b] <> NE.toList bs)
   in annihilateAll allPairs
annihilate (PRecord _ as) (PRecord _ bs) =
  let diffKeys = S.difference (M.keysSet as) (M.keysSet bs)
   in S.null diffKeys
        && annihilateAll (zip (M.elems as) (M.elems bs))
annihilate (PConstructor _ _ tyConA argsA) (PConstructor _ _ tyConB argsB) =
  (tyConA == tyConB)
    && annihilateAll
      (zip argsA argsB)
annihilate PString {} PString {} = True
annihilate (PTuple _ a as) _ =
  isComplete a && getAll (foldMap (All . isComplete) as)
annihilate (PRecord _ as) _ =
  foldr (\a total -> total && isComplete a) True as
annihilate (PArray _ itemsA (SpreadWildcard _)) (PArray _ itemsB (SpreadValue _ _)) =
  annihilateAll
    (zip itemsA itemsB)
annihilate (PArray _ itemsA (SpreadValue _ _)) (PArray _ itemsB (SpreadWildcard _)) =
  annihilateAll
    (zip itemsA itemsB)
annihilate _ _as = False

-- is this item total, as such, ie, is it always true?
isComplete :: Pattern var ann -> Bool
isComplete (PWildcard _) = True
isComplete (PVar _ _) = True
isComplete (PTuple _ a as) = isComplete a && getAll (foldMap (All . isComplete) (NE.toList as))
isComplete _ = False

redundantCases ::
  ( MonadError (TypeErrorF var Annotation) m,
    Eq var,
    Printer var,
    Printer (Pattern var Annotation),
    Show var
  ) =>
  Environment ->
  [Pattern var Annotation] ->
  m [Pattern var Annotation]
redundantCases env patterns = do
  generated <-
    mconcat
      <$> traverse (generate env) patterns
  let annihiliatePattern pat =
        filter
          ( not
              . annihilate
                (removeAnn pat)
              . removeAnn
          )
  -- add index, the first pattern is never redundant
  let patternsWithIndex = zip patterns ([0 ..] :: [Int])
  pure $
    snd $
      foldl'
        ( \(remaining, redundant) (pat, i) ->
            let rest = annihiliatePattern pat remaining
             in if length rest == length remaining && i > 0
                  then (rest, redundant <> [pat])
                  else (rest, redundant)
        )
        (generated, mempty)
        patternsWithIndex
