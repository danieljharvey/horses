module Language.Mimsa.Transform.Shared (extractIdentVar, repeatUntilEq) where

import Language.Mimsa.Types.AST

repeatUntilEq :: (Eq a) => (a -> a) -> a -> a
repeatUntilEq f a =
  let new = f a
   in if new == a then a else repeatUntilEq f new

extractIdentVar :: Identifier var ann -> var
extractIdentVar (Identifier _ name) = name
extractIdentVar (AnnotatedIdentifier _ name) = name
