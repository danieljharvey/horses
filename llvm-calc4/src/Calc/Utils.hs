module Calc.Utils (neZipWithM, neUnzip) where

-- useful junk goes here

import qualified Data.List.NonEmpty as NE
import Data.Bifunctor
import Control.Monad (zipWithM)

neZipWithM ::
  (Applicative m) =>
  (a -> b -> m c) ->
  NE.NonEmpty a ->
  NE.NonEmpty b ->
  m (NE.NonEmpty c)
neZipWithM f as bs =
  NE.fromList <$> zipWithM f (NE.toList as) (NE.toList bs)

neUnzip :: NE.NonEmpty (a, b) -> (NE.NonEmpty a, NE.NonEmpty b)
neUnzip = bimap NE.fromList NE.fromList . unzip . NE.toList

