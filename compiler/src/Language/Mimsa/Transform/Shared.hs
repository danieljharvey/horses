module Language.Mimsa.Transform.Shared (extractIdentVar, repeatUntilEq) where

import Language.Mimsa.Types.AST

repeatUntilEq :: (Eq a) => (a -> a) -> a -> a
repeatUntilEq f =
  let repeat' i a =
        let new = f a
         in if new == a || i < 1 then a else repeat' (i - 1) new
   in repeat' (10 :: Int)

extractIdentVar :: Identifier var ann -> var
extractIdentVar (Identifier _ name) = name
extractIdentVar (AnnotatedIdentifier _ name) = name
