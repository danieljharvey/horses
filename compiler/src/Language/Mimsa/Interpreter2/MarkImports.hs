module Language.Mimsa.Interpreter2.MarkImports (convertImports) where

import Control.Monad.Reader
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Interpreter.InterpretVar
import Language.Mimsa.Types.Store

convertImports :: StoreExpression ann -> Expr (InterpretVar Name) ann
convertImports se = runReader (markImports (storeExpression se)) initialState
  where
    initialState =
      ReaderState
        { rsBindings = getBindings (storeBindings se),
          rsHideVars = mempty
        }

data ReaderState var = ReaderState
  { rsBindings :: Map var ExprHash, -- map of names to look for, and exprHashes they point to
    rsHideVars :: Set var -- set of vars not to map as they have been introduced by lambdas etc
  }

type ImportM var a = Reader (ReaderState var) a

getVar :: (Ord var) => var -> Reader (ReaderState var) (InterpretVar var)
getVar var = do
  items <- asks rsBindings
  case M.lookup var items of
    Just exprHash -> pure (IImport exprHash)
    _ -> pure (ILocal var)

-- step through Expr, replacing vars with numbered variables
markImports ::
  (Ord var) =>
  Expr var ann ->
  ImportM var (Expr (InterpretVar var) ann)
markImports (MyVar ann from) =
  MyVar ann <$> getVar from
markImports (MyLet ann ident expr body) =
  MyLet ann <$> markIdentImports ident <*> markImports expr <*> markImports body
markImports (MyLetPattern ann pat expr body) =
  MyLetPattern ann <$> markPatternImports pat <*> markImports expr <*> markImports body
markImports (MyLiteral ann lit) =
  pure (MyLiteral ann lit)
markImports (MyInfix ann op a b) =
  MyInfix ann op <$> markImports a <*> markImports b
markImports (MyLambda ann ident body) =
  MyLambda ann <$> markIdentImports ident <*> markImports body
markImports (MyApp ann fn val) =
  MyApp ann <$> markImports fn <*> markImports val
markImports (MyIf ann predExpr thenExpr elseExpr) =
  MyIf ann <$> markImports predExpr <*> markImports thenExpr <*> markImports elseExpr
markImports (MyPair ann a b) =
  MyPair ann <$> markImports a <*> markImports b
markImports (MyRecord ann as) =
  MyRecord ann <$> traverse markImports as
markImports (MyRecordAccess ann recExpr name) =
  MyRecordAccess ann <$> markImports recExpr <*> pure name
markImports (MyArray ann as) =
  MyArray ann <$> traverse markImports as
markImports (MyDefineInfix ann op fn expr) =
  MyDefineInfix ann op <$> markImports fn <*> markImports expr
markImports (MyData ann dt expr) =
  MyData ann dt <$> markImports expr
markImports (MyConstructor ann const') =
  pure (MyConstructor ann const')
markImports (MyPatternMatch ann patExpr patterns) =
  let markPatterns (pat, pExpr) =
        (,) <$> markPatternImports pat <*> markImports pExpr
   in MyPatternMatch ann <$> markImports patExpr <*> traverse markPatterns patterns
markImports (MyTypedHole ann name) = pure (MyTypedHole ann name)

-- | TODO: recurse through all cases
markPatternImports ::
  Pattern var ann ->
  ImportM var (Pattern (InterpretVar var) ann)
markPatternImports pat =
  case pat of
    (PVar ann from') ->
      pure (PVar ann (ILocal from'))
    (PWildcard ann) ->
      pure $ PWildcard ann
    (PLit ann l) -> pure $ PLit ann l
    (PConstructor ann c d) -> PConstructor ann c <$> traverse markPatternImports d
    (PPair ann a b) ->
      PPair
        ann
        <$> markPatternImports a
        <*> markPatternImports b
    (PRecord ann as) ->
      PRecord ann <$> traverse markPatternImports as
    (PArray ann as a) ->
      PArray
        ann
        <$> traverse markPatternImports as
        <*> markSpreadNameImports a
    (PString ann as a) ->
      PString
        ann
        <$> markStringPartImports as
        <*> markStringPartImports a

markSpreadNameImports ::
  Spread var ann ->
  ImportM var (Spread (InterpretVar var) ann)
markSpreadNameImports (SpreadValue ann from') =
  pure (SpreadValue ann (ILocal from'))
markSpreadNameImports (SpreadWildcard a) = pure (SpreadWildcard a)
markSpreadNameImports NoSpread = pure NoSpread

markStringPartImports ::
  StringPart var ann ->
  ImportM var (StringPart (InterpretVar var) ann)
markStringPartImports (StrValue ann from') =
  pure (StrValue ann (ILocal from'))
markStringPartImports (StrWildcard ann) = pure (StrWildcard ann)

markIdentImports ::
  Identifier var ann ->
  ImportM var (Identifier (InterpretVar var) ann)
markIdentImports (Identifier ann from') =
  pure $ Identifier ann (ILocal from')
markIdentImports (AnnotatedIdentifier ann from') =
  pure $ AnnotatedIdentifier ann (ILocal from')
