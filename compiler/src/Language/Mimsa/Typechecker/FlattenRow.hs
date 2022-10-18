{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}

module Language.Mimsa.Typechecker.FlattenRow
  ( flattenRow,
  )
where

import Language.Mimsa.Types.Typechecker

-- these are tricky to deal with, so flatten them on the way in
flattenRow :: Type ann -> Type ann
flattenRow (MTRecord ann as (Just (MTRecord _ann' bs (Just rest)))) =
  flattenRow (MTRecord ann (as <> bs) (Just rest))
flattenRow other = other
