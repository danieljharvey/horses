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
        _ -> undefined
