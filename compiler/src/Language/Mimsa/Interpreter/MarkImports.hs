module Language.Mimsa.Interpreter.MarkImports (convertImports) where

import Control.Monad.Reader
import Data.Map (Map)
import qualified Data.Map as M
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Store

convertImports :: StoreExpression ann -> Expr (Name, Maybe ExprHash) ann
convertImports se = runReader (markImports (storeExpression se)) initialState
  where
    initialState =
      ReaderState
        { rsBindings = getBindings (storeBindings se)
        }

newtype ReaderState var = ReaderState
  { rsBindings :: Map var ExprHash -- map of names to look for, and exprHashes they point to
  }

type ImportM var a = Reader (ReaderState var) a

getVar :: (Ord var) => var -> Reader (ReaderState var) (var, Maybe ExprHash)
getVar var = do
  items <- asks rsBindings
  case M.lookup var items of
    Just exprHash -> pure (var, Just exprHash)
    _ -> pure (var, Nothing)

-- step through Expr, replacing vars with numbered variables
markImports ::
  (Ord var) =>
  Expr var ann ->
  ImportM var (Expr (var, Maybe ExprHash) ann)
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
  ImportM var (Pattern (var, Maybe ExprHash) ann)
markPatternImports pat =
  case pat of
    (PVar ann from') ->
      pure (PVar ann (from', Nothing))
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
  ImportM var (Spread (var, Maybe ExprHash) ann)
markSpreadNameImports (SpreadValue ann from') =
  pure (SpreadValue ann (from', Nothing))
markSpreadNameImports (SpreadWildcard a) = pure (SpreadWildcard a)
markSpreadNameImports NoSpread = pure NoSpread

markStringPartImports ::
  StringPart var ann ->
  ImportM var (StringPart (var, Maybe ExprHash) ann)
markStringPartImports (StrValue ann from') =
  pure (StrValue ann (from', Nothing))
markStringPartImports (StrWildcard ann) = pure (StrWildcard ann)

markIdentImports ::
  Identifier var ann ->
  ImportM var (Identifier (var, Maybe ExprHash) ann)
markIdentImports (Identifier ann from') =
  pure $ Identifier ann (from', Nothing)
markIdentImports (AnnotatedIdentifier ann from') =
  pure $ AnnotatedIdentifier ann (from', Nothing)
