{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Backend.Typescript.FromExpr where

import Data.Coerce (coerce)
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import Language.Mimsa.Backend.Typescript.Types
import Language.Mimsa.Printer
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Typechecker

toLiteral :: Literal -> TSLiteral
toLiteral lit = case lit of
  (MyInt i) -> TSInt i
  (MyBool b) -> TSBool b
  (MyString (StringType s)) -> TSString s

toPattern :: Pattern Name ann -> TSPattern
toPattern (PVar _ a) = TSPatternVar a
toPattern (PPair _ a b) = TSPatternPair (toPattern a) (toPattern b)
toPattern (PWildcard _) = TSPatternWildcard
toPattern (PConstructor _ name vars) = TSPatternConstructor name (toPattern <$> vars)
toPattern _ = undefined

-- | returns the type and any generics used in the expression
toTSType :: Type ann -> (TSType, Set TSGeneric)
toTSType (MTPrim _ MTString) = (TSType "String" [], mempty)
toTSType (MTVar _ a) =
  let newVar = case a of
        TVNum i' -> T.toTitle (prettyPrint i')
        TVName a' -> T.toTitle (coerce a')
   in (TSTypeVar newVar, S.singleton (TSGeneric newVar))
toTSType _ = undefined

newGenerics :: Set TSGeneric -> Set TSGeneric -> Set TSGeneric
newGenerics old new = S.difference new old

fromExpr :: Expr Name MonoType -> TSModule
fromExpr expr =
  TSModule (makeTSExpr mempty expr)
  where
    makeTSExpr generics expr' =
      case expr' of
        (MyLiteral _ lit) ->
          TSBody mempty (TSLit (toLiteral lit))
        (MyLet _ name letExpr letBody) ->
          let newLetExpr = makeTSExpr generics letExpr
              newBinding =
                TSAssignment
                  (TSPatternVar name)
                  (TSLetBody newLetExpr)
              (TSBody bindings' newExpr) = makeTSExpr generics letBody
           in TSBody ([newBinding] <> bindings') newExpr
        (MyLetPattern _ pat letExpr letBody) ->
          let newLetExpr = makeTSExpr generics letExpr
              newBinding =
                TSAssignment
                  (toPattern pat)
                  (TSLetBody newLetExpr)
              (TSBody bindings' newExpr) = makeTSExpr generics letBody
           in TSBody ([newBinding] <> bindings') newExpr
        (MyPair _ a b) ->
          let (TSBody _ tsA) = makeTSExpr generics a
              (TSBody _ tsB) = makeTSExpr generics b
           in TSBody mempty (TSArray [tsA, tsB])
        (MyVar _ a) -> TSBody mempty (TSVar a)
        (MyLambda argType bind body) ->
          let (mtArg, generics') = toTSType argType
              newGenerics' = newGenerics generics generics'
              tsBody = makeTSExpr (generics <> generics') body
           in TSBody
                []
                ( TSFunction
                    bind
                    newGenerics'
                    mtArg
                    ( TSFunctionBody tsBody
                    )
                )
        (MyPatternMatch _mtPatternMatch matchExpr patterns) ->
          let matches =
                ( \(pat, patExpr) ->
                    TSConditional
                      (toPattern pat)
                      (TSLetBody (makeTSExpr generics patExpr))
                )
                  <$> patterns
              (TSBody _ tsA) = makeTSExpr generics matchExpr
           in TSBody
                [ TSAssignment
                    (TSPatternVar "match")
                    ( TSLetBody
                        ( TSBody
                            matches
                            (TSError "Pattern match error")
                        )
                    )
                ]
                tsA
        (MyApp _mtApp func val) ->
          let (TSBody _ tsFunc) = makeTSExpr generics func
              (TSBody _ tsVal) = makeTSExpr generics val
           in TSBody [] (TSApp tsFunc tsVal)
        (MyConstructor _ tyCon) ->
          TSBody [] (TSData (prettyPrint tyCon) mempty)
        e -> error (show e)
