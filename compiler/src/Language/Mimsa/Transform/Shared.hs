module Language.Mimsa.Transform.Shared (extractIdentVar, repeatUntilEq, repeatUntilEqM) where

import Language.Mimsa.Core

repeatUntilEq :: (Eq a) => (a -> a) -> a -> a
repeatUntilEq f =
  let repeat' i a =
        let new = f a
         in if new == a || i < 1 then a else repeat' (i - 1) new
   in repeat' (10 :: Int)

repeatUntilEqM :: (Eq a, Monad m) => (a -> m a) -> a -> m a
repeatUntilEqM f =
  let repeat' i a = do
        new <- f a
        if new == a || i < 1 then pure a else repeat' (i - 1) new
   in repeat' (10 :: Int)

extractIdentVar :: Identifier var ann -> var
extractIdentVar (Identifier _ name) = name
