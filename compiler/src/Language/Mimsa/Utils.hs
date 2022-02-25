module Language.Mimsa.Utils (mapWithIndex, chainLookup) where

import Control.Applicative ((<|>))

-- I give in, it is time to throw all these things in a file

mapWithIndex :: (Int -> a -> b) -> [a] -> [b]
mapWithIndex f as =
  uncurry f <$> zip [1 ..] as

chainLookup :: (Eq a) => (a -> Maybe a) -> a -> Maybe a
chainLookup f a =
  case f a of
    Just a' ->
      if a == a'
        then Just a
        else chainLookup f a' <|> Just a'
    Nothing -> Nothing
