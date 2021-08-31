module Language.Mimsa.Backend.Typescript.FromExpr where

import Language.Mimsa.Backend.Typescript.Types
import Language.Mimsa.Typechecker.Elaborate
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers

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

fromExpr :: Expr Name TypedAnnotation -> TSModule
fromExpr expr =
  TSModule (makeTSExpr expr)
  where
    makeTSExpr expr' =
      case expr' of
        (MyLiteral _ lit) ->
          TSBody mempty (TSLit (toLiteral lit))
        (MyLet _ name letExpr letBody) ->
          let newLetExpr = makeTSExpr letExpr
              newBinding =
                TSAssignment
                  (TSPatternVar name)
                  (TSLetBody newLetExpr)
              (TSBody bindings' newExpr) = makeTSExpr letBody
           in TSBody ([newBinding] <> bindings') newExpr
        (MyLetPattern _ pat letExpr letBody) ->
          let newLetExpr = makeTSExpr letExpr
              newBinding =
                TSAssignment
                  (toPattern pat)
                  (TSLetBody newLetExpr)
              (TSBody bindings' newExpr) = makeTSExpr letBody
           in TSBody ([newBinding] <> bindings') newExpr
        (MyPair _ a b) ->
          let (TSBody _ tsA) = makeTSExpr a
              (TSBody _ tsB) = makeTSExpr b
           in TSBody mempty (TSArray [tsA, tsB])
        (MyVar _ a) -> TSBody mempty (TSVar a)
        _ -> undefined
