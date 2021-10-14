{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.Mimsa.Backend.Typescript.Patterns
  ( destructure,
    conditions,
  )
where

import Data.Foldable (foldl')
import qualified Data.Map as M
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import Language.Mimsa.Backend.Typescript.Types
import Language.Mimsa.Printer
import Language.Mimsa.Utils (mapWithIndex)

-- | Sometimes a destructuring is not useful but it is required
-- for instance in an array `[1,2,_,4]` the underscore is needed
-- but in [_] it is useless. We return (required, tsOutput) as a tuple
-- and try and remove useless matches
getDestructureExpr :: TSPattern -> (Bool, TSExpr, [TSStatement])
getDestructureExpr (TSPatternVar n) = (True, TSVar n, mempty)
getDestructureExpr TSPatternWildcard = (False, TSVar "_", mempty)
getDestructureExpr (TSPatternPair a b) =
  let (needA, tsA, assignA) = getDestructureExpr a
      (needB, tsB, assignB) = getDestructureExpr b
   in ( needA || needB,
        TSArray [TSArrayItem tsA, TSArrayItem tsB],
        assignA <> assignB
      )
getDestructureExpr (TSPatternRecord as) =
  let outputRecordItem (name, val) =
        let (useful, tsA, assignA) = getDestructureExpr val
         in if useful
              then (M.singleton name tsA, assignA)
              else (mempty, assignA)
      (items, assignAs) = mconcat $ outputRecordItem <$> M.toList as
   in ( not $ null items,
        TSRecord items,
        assignAs
      )
getDestructureExpr (TSPatternConstructor _ vars) =
  let (required, as, assignAs) = unzip3 (getDestructureExpr <$> vars)
   in ( or required,
        TSRecord
          ( M.singleton
              "vars"
              ( TSArray (TSArrayItem <$> as)
              )
          ),
        mconcat assignAs
      )
getDestructureExpr (TSPatternArray as spread) =
  let (spreadRequired, tsSpread) = case spread of
        TSSpreadValue a -> (True, [TSArraySpread (TSVar a)])
        _ -> (False, [])
      (required, tsAs, assignAs) = unzip3 (getDestructureExpr <$> as)
   in ( spreadRequired || or required,
        TSArray ((TSArrayItem <$> tsAs) <> tsSpread),
        mconcat assignAs
      )
getDestructureExpr (TSPatternLit _) = (False, TSVar "_", mempty)
getDestructureExpr (TSPatternString _tsHead _tsTail) =
  (False, TSVar "", mempty) -- TODO: return useful stuff in the 3rd slot

{-
  let aValue = case tsHead of
        TSStringVar vA ->
          [ TSInfix
              TSEquals
              ( TSApp
                  (TSRecordAccess "charAt" name)
                  (TSLit (TSInt 0))
              )
              (TSVar vA)
          ]
        _ -> mempty
      asValue = case tsTail of
        TSStringVar vAs ->
          [ TSInfix
              TSEquals
              ( TSApp
                  ( TSRecordAccess "slice" name
                  )
                  (TSLit (TSInt 1))
              )
              (TSVar vAs)
          ]
        _ -> mempty
   in (False, "")
-}

-- | Sometimes a destructuring is not useful but it is required
-- for instance in an array `[1,2,_,4]` the underscore is needed
-- but in [_] it is useless. We return (required, tsOutput) as a tuple
-- and try and remove useless matches
destructure :: TSPattern -> (Bool, Text)
destructure (TSPatternVar n) = (True, prettyPrint n)
destructure TSPatternWildcard = (False, "_")
destructure (TSPatternPair a b) =
  let (needA, tsA) = destructure a
      (needB, tsB) = destructure b
   in ( needA || needB,
        "[" <> tsA <> ","
          <> tsB
          <> "]"
      )
destructure (TSPatternRecord as) =
  let outputRecordItem (name, val) =
        let (useful, tsA) = destructure val
         in if useful
              then Just (prettyPrint name <> ": " <> tsA)
              else Nothing
      items = catMaybes $ outputRecordItem <$> M.toList as
   in ( not $ null items,
        "{ "
          <> T.intercalate
            ", "
            items
          <> " }"
      )
destructure (TSPatternConstructor _ vars) =
  let (required, as) = unzip (destructure <$> vars)
   in (or required, "{ vars: [" <> T.intercalate ", " as <> "] }")
destructure (TSPatternArray as spread) =
  let (spreadRequired, tsSpread) = case spread of
        TSSpreadValue a -> (True, ", ..." <> prettyPrint a)
        _ -> (False, "")
      (required, tsAs) = unzip (destructure <$> as)
   in (spreadRequired || or required, "[" <> T.intercalate ", " tsAs <> tsSpread <> "]")
destructure (TSPatternLit _) = (False, "_")
destructure (TSPatternString _tsHead _tsTail) =
  (False, "")

{-
  let aValue = case tsHead of
        TSStringVar vA ->
          [ TSInfix
              TSEquals
              ( TSApp
                  (TSRecordAccess "charAt" name)
                  (TSLit (TSInt 0))
              )
              (TSVar vA)
          ]
        _ -> mempty
      asValue = case tsTail of
        TSStringVar vAs ->
          [ TSInfix
              TSEquals
              ( TSApp
                  ( TSRecordAccess "slice" name
                  )
                  (TSLit (TSInt 1))
              )
              (TSVar vAs)
          ]
        _ -> mempty
   in (False, "")
-}
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
      subPattern i a = toMatchExpression (TSArrayAccess (i - 1) (TSRecordAccess "vars" name)) a
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
      subPattern i a =
        toMatchExpression (TSArrayAccess (i - 1) name) a
   in [lengthGuard] <> mconcat (mapWithIndex subPattern as)
toMatchExpression name (TSPatternString _a _as) =
  [ TSInfix
      TSGreaterThanOrEqualTo
      (TSRecordAccess "length" name)
      (TSLit (TSInt 1))
  ]
