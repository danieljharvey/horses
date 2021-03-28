{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Typechecker.Environment (getNativeConstructors, lookupConstructor, strType) where

import Control.Monad.Except
import Data.Map (Map)
import qualified Data.Map as M
import Language.Mimsa.Typechecker.TcMonad (TcMonad)
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Typechecker

builtInString :: MonoType
builtInString = MTData mempty "Str" mempty

builtInArray :: MonoType
builtInArray = MTData mempty "Arr" mempty

getNativeConstructors :: TyCon -> Maybe MonoType
getNativeConstructors "StrHead" = Just builtInString
getNativeConstructors "StrEmpty" = Just builtInString
getNativeConstructors "ArrHead" = Just builtInArray
getNativeConstructors "ArrEmpty" = Just builtInArray
getNativeConstructors _ = Nothing

getAllDataTypes :: Environment -> Map TyCon (DataType Annotation)
getAllDataTypes env =
  M.fromList
    [ ( "Str",
        strType
      ),
      ( "Arr",
        arrType
      )
    ]
    <> getDataTypes env

-- Str is the datatype for case matches
strType :: DataType Annotation
strType =
  DataType
    "Str"
    mempty
    ( M.fromList
        [ ("StrHead", [MTPrim mempty MTString, MTPrim mempty MTString]),
          ("StrEmpty", mempty)
        ]
    )

-- Arr is the datatype for case matches
arrType :: DataType Annotation
arrType =
  DataType
    "Arr"
    mempty
    ( M.fromList
        [ ( "ArrHead",
            [ MTArray mempty (MTPrim mempty MTString),
              MTArray mempty (MTPrim mempty MTString)
            ]
          ),
          ("ArrEmpty", mempty)
        ]
    )

-- given a constructor name, return the type it lives in
lookupConstructor ::
  Environment ->
  Annotation ->
  TyCon ->
  TcMonad (DataType Annotation)
lookupConstructor env ann name =
  case M.toList $ M.filter (containsConstructor name) (getAllDataTypes env) of
    [(_, a)] -> pure a -- we only want a single match
    (_ : _) -> throwError (ConflictingConstructors ann name)
    _ -> throwError (TypeConstructorNotInScope env ann name)

-- does this data type contain the given constructor?
containsConstructor :: TyCon -> DataType ann -> Bool
containsConstructor name (DataType _tyName _tyVars constructors) =
  M.member name constructors
