{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.Mimsa.Backend.Typescript.Patterns
  ( conditions,
    getDestructureExpr,
    destructure,
  )
where

import Data.Foldable (foldl')
import qualified Data.Map as M
import Language.Mimsa.Backend.Typescript.Types
import Language.Mimsa.Printer
import Language.Mimsa.Utils (mapWithIndex)

isUseful :: TSExpr -> Bool
isUseful TSUnderscore = False
isUseful _ = True

-- | matchExpr is the thing we're match against, useful when constructing
-- string destructurings
getDestructureExpr :: TSExpr -> TSPattern -> (TSExpr, [TSStatement])
getDestructureExpr _ (TSPatternVar n) = (TSVar n, mempty)
getDestructureExpr _ TSPatternWildcard = (TSUnderscore, mempty)
getDestructureExpr matchExpr (TSPatternPair a b) =
  let (tsA, assignA) = getDestructureExpr (TSArrayAccess 0 matchExpr) a
      (tsB, assignB) = getDestructureExpr (TSArrayAccess 1 matchExpr) b
   in ( TSArray [TSArrayItem tsA, TSArrayItem tsB],
        assignA <> assignB
      )
getDestructureExpr matchExpr (TSPatternRecord as) =
  let outputRecordItem (name, val) =
        let (tsA, assignA) = getDestructureExpr (TSRecordAccess name matchExpr) val
         in if isUseful tsA
              then (M.singleton name tsA, assignA)
              else (mempty, assignA)
      (items, assignAs) = mconcat $ outputRecordItem <$> M.toList as
   in ( if null items then TSUnderscore else TSRecord items,
        assignAs
      )
getDestructureExpr matchExpr (TSPatternConstructor _ vars) =
  let withIndex i =
        getDestructureExpr
          ( TSArrayAccess
              (i - 1)
              (TSRecordAccess "vars" matchExpr)
          )
      (as, assignAs) = unzip (mapWithIndex withIndex vars)
   in ( if or (isUseful <$> as)
          then
            TSRecord
              ( M.singleton
                  "vars"
                  ( TSArray (TSArrayItem <$> as)
                  )
              )
          else TSUnderscore,
        mconcat assignAs
      )
getDestructureExpr matchExpr (TSPatternArray as spread) =
  let (spreadRequired, tsSpread) = case spread of
        TSSpreadValue a -> (True, [TSArraySpread (TSVar a)])
        _ -> (False, [])
      withIndex i = getDestructureExpr (TSArrayAccess (i - 1) matchExpr)
      (tsAs, assignAs) = unzip (mapWithIndex withIndex as)
   in ( if spreadRequired || or (isUseful <$> tsAs)
          then TSArray ((TSArrayItem <$> tsAs) <> tsSpread)
          else TSUnderscore,
        mconcat assignAs
      )
getDestructureExpr _ (TSPatternLit _) = (TSUnderscore, mempty)
getDestructureExpr matchExpr (TSPatternString tsHead tsTail) =
  let aValue = case tsHead of
        TSStringVar vA ->
          [ TSAssignment
              (TSVar vA)
              Nothing
              ( TSLetBody
                  ( TSBody
                      mempty
                      ( TSApp
                          (TSRecordAccess "charAt" matchExpr)
                          (TSLit (TSInt 0))
                      )
                  )
              )
          ]
        _ -> mempty
      asValue = case tsTail of
        TSStringVar vAs ->
          [ TSAssignment
              (TSVar vAs)
              Nothing
              ( TSLetBody
                  ( TSBody
                      mempty
                      ( TSApp
                          ( TSRecordAccess "slice" matchExpr
                          )
                          (TSLit (TSInt 1))
                      )
                  )
              )
          ]
        _ -> mempty
   in (TSUnderscore, aValue <> asValue)

destructure :: TSPattern -> [TSStatement]
destructure tsPat =
  let (patExpr, statements) = getDestructureExpr (TSVar "value") tsPat
      tsMainConst =
        if isUseful patExpr
          then
            [ TSAssignment
                patExpr
                Nothing
                ( TSLetBody
                    ( TSBody
                        mempty
                        (TSVar "value")
                    )
                )
            ]
          else mempty
   in tsMainConst <> statements

conditions :: TSPattern -> TSExpr
conditions pat =
  let parts = toMatchExpression (TSVar "value") pat
   in case parts of
        [] -> TSLit (TSBool True)
        (a : as) -> foldl' (TSInfix TSAnd) a as

-- | turn a pattern map into a match expression for this pattern
toMatchExpression :: TSExpr -> TSPattern -> [TSExpr]
toMatchExpression _ TSPatternWildcard =
  mempty
toMatchExpression _ (TSPatternVar _) =
  mempty
toMatchExpression name (TSPatternPair a b) =
  toMatchExpression (TSArrayAccess 0 name) a
    <> toMatchExpression (TSArrayAccess 1 name) b
toMatchExpression name (TSPatternLit lit) =
  [TSInfix TSEquals name (TSLit lit)]
toMatchExpression name (TSPatternRecord items) =
  let subPattern (k, v) = toMatchExpression (TSRecordAccess k name) v
   in mconcat (subPattern <$> M.toList items)
toMatchExpression name (TSPatternConstructor tyCon args) =
  let tyConGuard = TSInfix TSEquals (TSRecordAccess "type" name) (TSLit (TSString (prettyPrint tyCon)))
      subPattern i = toMatchExpression (TSArrayAccess (i - 1) (TSRecordAccess "vars" name))
   in [tyConGuard] <> mconcat (mapWithIndex subPattern args)
toMatchExpression name (TSPatternArray as spread) =
  let lengthGuard = case spread of
        TSNoSpread ->
          TSInfix
            TSEquals
            (TSRecordAccess "length" name)
            (TSLit (TSInt (length as)))
        TSSpreadWildcard ->
          TSInfix
            TSGreaterThanOrEqualTo
            (TSRecordAccess "length" name)
            (TSLit (TSInt (length as)))
        (TSSpreadValue _) ->
          TSInfix
            TSGreaterThanOrEqualTo
            (TSRecordAccess "length" name)
            (TSLit (TSInt (length as)))
      subPattern i =
        toMatchExpression (TSArrayAccess (i - 1) name)
   in [lengthGuard] <> mconcat (mapWithIndex subPattern as)
toMatchExpression name (TSPatternString _a _as) =
  [ TSInfix
      TSGreaterThanOrEqualTo
      (TSRecordAccess "length" name)
      (TSLit (TSInt 1))
  ]
