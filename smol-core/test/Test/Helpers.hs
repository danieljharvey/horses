{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Helpers
  ( tyBool,
    tyBoolLit,
    tyInt,
    tyIntLit,
    tyStrLit,
    tyUnit,
    tyVar,
    tyUnknown,
    tyTuple,
    tyCons,
    tyFunc,
    tyString,
    bool,
    int,
    var,
    tuple,
    array,
    unit,
    identifier,
    constructor,
    patternMatch,
    unsafeParseExpr,
    unsafeParseModule,
    unsafeParseModuleItems,
    unsafeParseType,
    unsafeParseTypedExpr,
    joinText,
    runTypecheckM,
    typecheckEnv,
    showTypeclass,
    eqTypeclass,
    unsafeParseInstanceExpr,
    tcVar,
    typeForComparison,
    testModule,
  )
where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Foldable (foldl')
import Data.Functor
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import qualified Data.Sequence as Seq
import qualified Data.Set as S
import qualified Data.Set.NonEmpty as NES
import Data.Text (Text)
import qualified Data.Text as T
import Smol.Core
import Smol.Core.Modules.FromParts
import Smol.Core.Modules.ResolveDeps
import Smol.Core.Modules.Typecheck
import Smol.Core.Modules.Types.Module
import Smol.Core.Modules.Types.ModuleError
import Smol.Core.Modules.Types.ModuleItem
import Smol.Core.Typecheck.FromParsedExpr
import Test.BuiltInTypes (builtInTypes)

typedModule ::
  (MonadError (ModuleError Annotation) m) =>
  T.Text ->
  m (Module ResolvedDep (Type ResolvedDep Annotation))
typedModule input = do
  let moduleItems = case parseModuleAndFormatError input of
        Right a -> a
        _ -> error "parsing module for typeclass spec"
  myModule <- moduleFromModuleParts moduleItems

  let typeClasses = resolveTypeclass <$> moClasses myModule
      typeclassMethods = S.fromList . M.elems . fmap tcFuncName $ typeClasses

  (resolvedModule, deps) <-
    modifyError ErrorInResolveDeps (resolveModuleDeps typeclassMethods myModule)

  typecheckModule input resolvedModule deps

getRight :: (Show e) => Either e a -> a
getRight (Right a) = a
getRight (Left e) = error (show e)

testModule :: Module ResolvedDep (Type ResolvedDep Annotation)
testModule =
  getRight $
    typedModule $
      joinText
        [ "class Eq a { equals: a -> a -> Bool }",
          "instance Eq Int = \\a -> \\b -> a == b",
          "instance Eq Bool = \\a -> \\b -> a == b",
          "instance Eq String = \\a -> \\b -> a == b",
          "instance (Eq a, Eq b) => Eq (a,b) = ",
          "\\pairA -> \\pairB -> case (pairA, pairB) of ((a1, b1), (a2, b2)) -> ",
          "if equals a1 a2 then equals b1 b2 else False"
        ]

tyBool :: (Monoid ann) => Type dep ann
tyBool = TPrim mempty TPBool

tyBoolLit :: (Monoid ann) => Bool -> Type dep ann
tyBoolLit = TLiteral mempty . TLBool

tyInt :: (Monoid ann) => Type dep ann
tyInt = TPrim mempty TPInt

tyUnit :: (Monoid ann) => Type dep ann
tyUnit = TLiteral mempty TLUnit

tyString :: (Monoid ann) => Type dep ann
tyString = TPrim mempty TPString

tyIntLit :: (Monoid ann) => [Integer] -> Type dep ann
tyIntLit = TLiteral mempty . TLInt . NES.fromList . NE.fromList

tyStrLit :: (Monoid ann) => [Text] -> Type dep ann
tyStrLit = TLiteral mempty . TLString . NES.fromList . NE.fromList

tyVar :: (Monoid ann) => Text -> Type ParseDep ann
tyVar = TVar mempty . emptyParseDep . Identifier

tyUnknown :: (Monoid ann) => Integer -> Type dep ann
tyUnknown = TUnknown mempty

tyTuple ::
  (Monoid ann) =>
  Type dep ann ->
  [Type dep ann] ->
  Type dep ann
tyTuple a as = TTuple mempty a (NE.fromList as)

tyCons ::
  (Monoid ann) =>
  TypeName ->
  [Type ParseDep ann] ->
  Type ParseDep ann
tyCons typeName =
  foldl' (TApp mempty) (TConstructor mempty (emptyParseDep typeName))

tyFunc :: (Monoid ann, Ord (dep Identifier)) => Type dep ann -> Type dep ann -> Type dep ann
tyFunc = TFunc mempty mempty

unit :: (Monoid ann) => Expr dep ann
unit = EPrim mempty PUnit

bool :: (Monoid ann) => Bool -> Expr dep ann
bool = EPrim mempty . PBool

int :: (Monoid ann) => Integer -> Expr dep ann
int = EPrim mempty . PInt

var :: (Monoid ann) => Text -> Expr ParseDep ann
var = EVar mempty . emptyParseDep . Identifier

tuple ::
  (Monoid ann) =>
  Expr dep ann ->
  [Expr dep ann] ->
  Expr dep ann
tuple a as = ETuple mempty a (NE.fromList as)

constructor ::
  (Monoid ann) =>
  Text ->
  Expr ParseDep ann
constructor lbl = EConstructor mempty (emptyParseDep (Constructor lbl))

identifier :: Text -> ParseDep Identifier
identifier = emptyParseDep . Identifier

patternMatch ::
  (Monoid ann) =>
  Expr dep ann ->
  [(Pattern dep ann, Expr dep ann)] ->
  Expr dep ann
patternMatch expr pats =
  EPatternMatch mempty expr (NE.fromList pats)

array :: (Monoid ann) => [Expr dep ann] -> Expr dep ann
array as = EArray mempty (Seq.fromList as)

------

unsafeParseExpr :: Text -> Expr ParseDep ()
unsafeParseExpr input = case parseExprAndFormatError input of
  Right expr -> expr $> ()
  Left e -> error (show e)

unsafeParseType :: Text -> Type ParseDep ()
unsafeParseType input = case parseTypeAndFormatError input of
  Right ty -> ty $> ()
  Left e -> error (show e)

unsafeParseModule :: Text -> Module ParseDep ()
unsafeParseModule input =
  case moduleFromModuleParts (unsafeParseModuleItems input) of
    Right a -> a $> ()
    Left e -> error (show e)

unsafeParseModuleItems :: Text -> [ModuleItem ()]
unsafeParseModuleItems input = case parseModuleAndFormatError input of
  Right parts -> fmap void parts
  Left e -> error (show e)

-- | parse a typed expr, ie parse it and fill the type with crap
unsafeParseTypedExpr :: Text -> ResolvedExpr (Type ResolvedDep Annotation)
unsafeParseTypedExpr input = case parseExprAndFormatError input of
  Right expr -> fromParsedExpr expr $> TPrim mempty TPBool
  Left e -> error (show e)

joinText :: [T.Text] -> T.Text
joinText = T.intercalate "\n"

----

runTypecheckM ::
  (Monad m) =>
  TCEnv ann ->
  StateT (TCState ann) (WriterT [TCWrite ann] (ReaderT (TCEnv ann) m)) a ->
  m a
runTypecheckM env action =
  fst
    <$> runReaderT
      ( runWriterT
          ( evalStateT
              action
              (TCState mempty 0 mempty)
          )
      )
      env

------

tcVar :: (Monoid ann) => Identifier -> Type ResolvedDep ann
tcVar = TVar mempty . LocalDefinition

showTypeclass :: (Monoid ann) => Typeclass ResolvedDep ann
showTypeclass =
  Typeclass
    { tcConstraints = mempty,
      tcName = "Show",
      tcArgs = ["a"],
      tcFuncName = "show",
      tcFuncType = tyFunc (tcVar "a") tyString
    }

eqTypeclass :: (Monoid ann) => Typeclass ResolvedDep ann
eqTypeclass =
  Typeclass
    { tcConstraints = mempty,
      tcName = "Eq",
      tcArgs = ["a"],
      tcFuncName = "equals",
      tcFuncType = tyFunc (tcVar "a") (tyFunc (tcVar "a") tyBool)
    }

classes :: (Monoid ann) => M.Map TypeclassName (Typeclass ResolvedDep ann)
classes =
  M.fromList
    [ ("Eq", eqTypeclass),
      ("Show", showTypeclass)
    ]

unsafeParseInstanceExpr :: (Monoid ann) => Text -> Expr ResolvedDep ann
unsafeParseInstanceExpr =
  fmap (const mempty) . fromParsedExpr . unsafeParseExpr

instances :: (Ord ann, Monoid ann) => M.Map (Constraint ResolvedDep ann) (Instance ResolvedDep ann)
instances =
  M.fromList
    [ ( Constraint "Eq" [tyInt],
        Instance {inExpr = unsafeParseInstanceExpr "\\a -> \\b -> a == b", inConstraints = []}
      ),
      ( Constraint "Eq" [tyTuple (tcVar "a") [tcVar "b"]],
        Instance
          { inExpr =
              unsafeParseInstanceExpr "\\a -> \\b -> case (a,b) of ((a1, a2), (b1, b2)) -> if equals a1 b1 then equals a2 b2 else False",
            inConstraints =
              [ Constraint "Eq" [tcVar "a"],
                Constraint "Eq" [tcVar "b"]
              ]
          }
      )
    ]

typecheckEnv :: (Monoid ann, Ord ann) => TCEnv ann
typecheckEnv =
  TCEnv
    mempty
    (builtInTypes emptyResolvedDep)
    classes
    instances
    mempty

----

-- simplify type for equality check
-- remove anything that can't be described in a type signature
typeForComparison :: (Ord (dep Identifier)) => Type dep ann -> Type dep ann
typeForComparison (TFunc ann _ fn arg) =
  TFunc ann mempty (typeForComparison fn) (typeForComparison arg)
typeForComparison (TArray ann _ as) = TArray ann 0 (typeForComparison as)
typeForComparison other = mapType typeForComparison other
