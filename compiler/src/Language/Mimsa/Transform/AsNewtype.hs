module Language.Mimsa.Transform.AsNewtype (asNewtype) where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Language.Mimsa.ExprUtils
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers

asNewtype :: Map TyCon DataType -> Expr var ann -> Expr var ann
asNewtype dts = asNewtypeInternal matchCons
  where
    -- Set of TyCons for types with one constructor
    matchCons =
      M.keysSet (M.filter (\(DataType _ _ args) -> length args == 1) dts)

-- look for Constructors or destructuring of Constructors
-- if it's single Constructor remove it
-- 1. `let (Parser inner) = parser in ...`
-- ...becomes `let inner = parser in ...`
--
-- 2. `Parser p` becomes just `p`
asNewtypeInternal :: Set TyCon -> Expr var ann -> Expr var ann
asNewtypeInternal tcs (MyApp _ (MyApp ann (MyConstructor _ consName) valA) valB)
  | S.member consName tcs =
    MyPair ann (asNewtypeInternal tcs valA) (asNewtypeInternal tcs valB)
asNewtypeInternal tcs (MyApp _ (MyConstructor _ consName) val)
  | S.member consName tcs =
    asNewtypeInternal tcs val
asNewtypeInternal tcs (MyLetPattern ann (PConstructor _ consName [a]) expr body)
  | S.member consName tcs =
    MyLetPattern ann a (asNewtypeInternal tcs expr) (asNewtypeInternal tcs body)
asNewtypeInternal tcs (MyLetPattern ann (PConstructor pAnn consName [a, b]) expr body)
  | S.member consName tcs =
    MyLetPattern ann (PPair pAnn a b) (asNewtypeInternal tcs expr) (asNewtypeInternal tcs body)
asNewtypeInternal tcs (MyPatternMatch ann expr [(PConstructor _ consName [a], patExpr)])
  | S.member consName tcs =
    MyPatternMatch ann (asNewtypeInternal tcs expr) [(a, asNewtypeInternal tcs patExpr)]
asNewtypeInternal tcs (MyPatternMatch ann expr [(PConstructor pAnn consName [a, b], patExpr)])
  | S.member consName tcs =
    MyPatternMatch ann (asNewtypeInternal tcs expr) [(PPair pAnn a b, asNewtypeInternal tcs patExpr)]
asNewtypeInternal tcs other = mapExpr (asNewtypeInternal tcs) other
