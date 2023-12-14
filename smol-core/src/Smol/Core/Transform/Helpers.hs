module Smol.Core.Transform.Helpers (repeatUntilEq) where

repeatUntilEq :: (Eq a) => (a -> a) -> a -> a
repeatUntilEq f =
  let repeat' i a =
        let new = f a
         in if new == a || i < 1 then a else repeat' (i - 1) new
   in repeat' (10 :: Int)
