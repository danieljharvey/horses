{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}

module Language.Mimsa.Typechecker.KindChecker (kindCheck, Kind (..)) where

import qualified Data.Map as M
import qualified Data.Set as S
import GHC.Generics
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Typechecker

data Kind var
  = KindType
  | KindArrow (Kind var) (Kind var)
  | KindVar var
  deriving stock (Generic, Show, Eq, Ord)

kindCheck :: Environment -> MonoType -> Kind var
kindCheck _ (MTPrim _ _) = KindType
kindCheck _ (MTVar _ _a) = undefined
kindCheck _ MTFunction {} = KindType
kindCheck _ MTPair {} = KindType
kindCheck _ (MTRecord _ _) = KindType
kindCheck env mt =
  case varsFromDataType mt of
    Just (name, vars) ->
      let countVars =
            length
              ( filter
                  ( \case
                      MTVar {} -> False
                      _ -> True
                  )
                  vars
              )
       in case findDataType name env of
            Just (DataType _ typeArgs _) ->
              let complexity = length typeArgs - countVars
               in foldr
                    ( \_ a ->
                        KindArrow a KindType
                    )
                    KindType
                    [1 .. complexity]
            _ -> error "oh no"
    Nothing -> error "oh no"

findDataType :: TyCon -> Environment -> Maybe DataType
findDataType tyCon env =
  let match = M.filter (\(DataType _ _ tyCons) -> S.member tyCon (M.keysSet tyCons)) (getDataTypes env)
   in if M.null match
        then Nothing
        else Just (snd $ M.findMin match)
