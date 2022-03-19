module Language.Mimsa.Utils (mapWithIndex) where

-- I give in, it is time to throw all these things in a file

mapWithIndex :: (Int -> a -> b) -> [a] -> [b]
mapWithIndex f as =
  uncurry f <$> zip [1 ..] as
