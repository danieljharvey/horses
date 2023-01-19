{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Smol.Core.Typecheck.Exhaustiveness
  ( isExhaustive,
    redundantCases,
    annihilate,
    validatePatterns,
    noDuplicateVariables,
    smallerListVersions,
  )
where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Bifunctor (first)
import Data.Foldable
import Data.Functor
import Data.List (nub)
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Monoid
import qualified Data.Set as S
import Smol.Core.Typecheck.Shared
import Smol.Core.Typecheck.Types
import Smol.Core.Types
import Smol.Core.Types.PatternMatchError
import Smol.Core.Types.ResolvedDep

validatePatterns ::
  ( MonadError (TCError Annotation) m,
    MonadReader (TCEnv Annotation) m
  ) =>
  Type Annotation ->
  [Pattern ResolvedDep (Type Annotation)] ->
  m ()
validatePatterns ann patterns = do
  traverse_ noDuplicateVariables patterns
  missing <- isExhaustive patterns
  _ <- case missing of
    [] -> pure ()
    _ ->
      throwError (TCPatternMatchError $ MissingPatterns ann missing)
  redundant <- redundantCases patterns
  case redundant of
    [] -> pure ()
    _ ->
      throwError (TCPatternMatchError $ RedundantPatterns ann redundant)

noDuplicateVariables ::
  ( MonadError (TCError Annotation) m
  ) =>
  Pattern ResolvedDep (Type Annotation) ->
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
            ( TCPatternMatchError $
                DuplicateVariableUse
                  (getPatternAnnotation pat)
                  dupes
            )

getVariables ::
  Pattern ResolvedDep (Type Annotation) ->
  Map (ResolvedDep Identifier) Int
getVariables (PWildcard _) = mempty
getVariables (PLiteral _ _) = mempty
getVariables (PVar _ a) = M.singleton a 1
getVariables (PTuple _ a as) =
  M.unionWith (+) (getVariables a) (foldMap getVariables as)
{-getVariables (PRecord _ as) =
foldr (M.unionWith (+)) mempty (getVariables <$> as) -}
{-getVariables (PArray _ as spread) =
let vars = [getSpreadVariables spread] <> (getVariables <$> as)
 in foldr (M.unionWith (+)) mempty vars -}
getVariables (PConstructor _ _ args) =
  foldr (M.unionWith (+)) mempty (getVariables <$> args)

{-getVariables (PString _ a as) =
  M.unionWith (+) (getStringPartVariables a) (getStringPartVariables as)
-}

{-
getSpreadVariables :: (Ord var) => Spread var Annotation -> Map var Int
getSpreadVariables (SpreadValue _ a) = M.singleton a 1
getSpreadVariables _ = mempty

getStringPartVariables :: (Ord var) => StringPart var Annotation -> Map var Int
getStringPartVariables (StrWildcard _) = mempty
getStringPartVariables (StrValue _ a) = M.singleton a 1
-}

-- | given a list of patterns, return a list of missing patterns
isExhaustive ::
  ( MonadError (TCError Annotation) m,
    MonadReader (TCEnv Annotation) m
  ) =>
  [Pattern ResolvedDep (Type Annotation)] ->
  m [Pattern ResolvedDep (Type Annotation)]
isExhaustive patterns = do
  generated <- mconcat <$> traverse generate patterns
  pure $ filterMissing patterns generated

generate ::
  ( MonadError (TCError Annotation) m,
    MonadReader (TCEnv Annotation) m
  ) =>
  Pattern ResolvedDep (Type Annotation) ->
  m [Pattern ResolvedDep (Type Annotation)]
generate pat = (<>) [pat] <$> generateFromPattern pat

-- | Given a pattern, generate others required for it
generateFromPattern ::
  ( MonadError (TCError Annotation) m,
    MonadReader (TCEnv Annotation) m
  ) =>
  Pattern ResolvedDep (Type Annotation) ->
  m [Pattern ResolvedDep (Type Annotation)]
generateFromPattern (PLiteral ty _) = generateFromType ty
generateFromPattern (PWildcard _) = pure mempty
generateFromPattern (PVar _ _) = pure mempty
generateFromPattern (PTuple ty a as) = do
  genAs <- traverse (\pat -> NE.toList <$> generateAlways (getPatternAnnotation pat) pat) (NE.cons a as)
  let tuple ne = PTuple ty (NE.head ne) (NE.fromList $ NE.tail ne)
  pure (tuple <$> sequence genAs)
generateFromPattern (PConstructor ty _constructor args) = do
  (typeName, _args) <- flattenConstructorType ty
  dt <- lookupTypeName typeName
  _newFromArgs <- traverse generateFromPattern args
  newDataTypes <- requiredFromDataType dt
  let newCons = mempty -- PConstructor mempty tyCon <$> sequence newFromArgs
  pure (newCons <> newDataTypes)

-- | used for tuples and constructors args
-- (any "product" really)
-- given (a,b) , return [(a, gennedForB1), (a, gennedForB), (gennedForA1, b),
-- (gennedForA2, b)]
_generateMany ::
  NE.NonEmpty (Pattern ResolvedDep (Type Annotation)) ->
  m [NE.NonEmpty (Pattern ResolvedDep (Type Annotation))]
_generateMany = undefined

-- | Given a type, generate patterns, useful for literals where the type is
-- most important
generateFromType ::
  ( MonadError (TCError Annotation) m -- ,
  -- MonadReader (TCEnv Annotation) m
  ) =>
  Type Annotation ->
  m [Pattern ResolvedDep (Type Annotation)]
generateFromType ty@(TLiteral _ literal) =
  pure [PLiteral ty (primFromTypeLiteral literal)]
generateFromType ty@(TPrim _ TPBool) =
  pure
    [ PLiteral ty (PBool True),
      PLiteral ty (PBool False)
    ]
generateFromType ty@(TPrim _ TPInt) =
  pure [PWildcard ty]
generateFromType ty@(TPrim _ TPNat) =
  pure [PWildcard ty]
generateFromType (TUnion _ a b) =
  mappend
    <$> generateFromType a
    <*> generateFromType b
generateFromType _ = pure mempty

generateAlways ::
  (MonadError (TCError Annotation) m, MonadReader (TCEnv Annotation) m) =>
  Type Annotation ->
  Pattern ResolvedDep (Type Annotation) ->
  m (NE.NonEmpty (Pattern ResolvedDep (Type Annotation)))
generateAlways ty pat = do
  generated <- generateFromPattern pat
  case NE.nonEmpty generated of
    Nothing -> pure (NE.singleton (PWildcard ty))
    Just other -> pure other

-- given a list [[1,2,3]], return [[1,2,3], [1,2], [1]]
smallerListVersions :: [[a]] -> [[a]]
smallerListVersions aas =
  let get x = case x of
        [] -> []
        (_ : as) -> get as <> [x]
   in get =<< aas

requiredFromDataType ::
  (MonadError (TCError Annotation) m) =>
  DataType Annotation ->
  m [Pattern ResolvedDep (Type Annotation)]
requiredFromDataType (DataType _ _ cons) =
  if length cons < 2 -- if there is only one constructor don't generate more
    then pure mempty
    else do
      let wrongValueThatTypechecks = TPrim mempty TPInt -- TODO: wrong, we need to apply args to type
          new (n, as) =
            [ PConstructor
                wrongValueThatTypechecks
                n
                (PWildcard wrongValueThatTypechecks <$ as)
            ]
      pure $ mconcat (new . first LocalDefinition <$> M.toList cons)

-- filter outstanding items
filterMissing ::
  (Eq ann) =>
  [Pattern ResolvedDep (Type ann)] ->
  [Pattern ResolvedDep (Type ann)] ->
  [Pattern ResolvedDep (Type ann)]
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

removeAnn :: Pattern ResolvedDep ann -> Pattern ResolvedDep ()
removeAnn p = p $> ()

-- does left pattern satisfy right pattern?
annihilateAll ::
  [(Pattern ResolvedDep (), Pattern ResolvedDep ())] ->
  Bool
annihilateAll =
  foldr
    (\(a, b) keep -> keep && annihilate a b)
    True

-- | if left is on the right, should we get rid?
annihilate :: Pattern ResolvedDep () -> Pattern ResolvedDep () -> Bool
annihilate l r =
  case (l, r) of
    (a, b) | a == b -> True
    (PWildcard _, _) -> True -- wildcard trumps all
    (PVar _ _, _) -> True -- as does var
    (PTuple _ a as, PTuple _ b bs) ->
      let allPairs = zip ([a] <> NE.toList as) ([b] <> NE.toList bs)
       in annihilateAll allPairs
    (PConstructor _ tyConA argsA, PConstructor _ tyConB argsB) ->
      (tyConA == tyConB)
        && annihilateAll
          (zip argsA argsB)
    (PTuple _ a as, _) ->
      isComplete a && getAll (foldMap (All . isComplete) as)
    _ -> False

-- is this item total, as such, ie, is it always true?
isComplete :: Pattern ResolvedDep ann -> Bool
isComplete (PWildcard _) = True
isComplete (PVar _ _) = True
isComplete (PTuple _ a as) = isComplete a && getAll (foldMap (All . isComplete) (NE.toList as))
isComplete _ = False

redundantCases ::
  ( MonadError (TCError Annotation) m,
    MonadReader (TCEnv Annotation) m
  ) =>
  [Pattern ResolvedDep (Type Annotation)] ->
  m [Pattern ResolvedDep (Type Annotation)]
redundantCases patterns = do
  generated <- mconcat <$> traverse generate patterns
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
