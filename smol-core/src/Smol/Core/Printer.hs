{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Smol.Core.Printer
  ( Printer (..),
    renderWithWidth,
  )
where

-- the Printer type class is used for internal debugging
-- prettyDoc returns a Prettyprinter doc for nicer output

import Control.Monad.Identity
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Set.NonEmpty as NES
import Data.Text (Text)
import GHC.Natural
import Prettyprinter
import Prettyprinter.Render.Text

renderWithWidth :: Int -> Doc ann -> Text
renderWithWidth w doc = renderStrict (layoutPretty layoutOptions (unAnnotate doc))
  where
    layoutOptions = LayoutOptions {layoutPageWidth = AvailablePerLine w 1}

class Printer a where
  prettyDoc :: a -> Doc ann

instance (Printer a) => Printer (Identity a) where
  prettyDoc (Identity a) = prettyDoc a

instance (Printer e, Printer a) => Printer (Either e a) where
  prettyDoc (Left e) = prettyDoc e
  prettyDoc (Right a) = prettyDoc a

instance Printer () where
  prettyDoc = const ""

instance (Printer a) => Printer (Maybe a) where
  prettyDoc (Just a) = prettyDoc a
  prettyDoc _ = mempty

instance {-# OVERLAPPING #-} Printer [Char] where
  prettyDoc = pretty

instance Printer Text where
  prettyDoc = pretty

instance Printer Bool where
  prettyDoc = pretty

instance Printer Int where
  prettyDoc = pretty

instance Printer Natural where
  prettyDoc = pretty

instance (Printer a) => Printer [a] where
  prettyDoc = sep . fmap prettyDoc

instance (Printer k, Printer v) => Printer (Map k v) where
  prettyDoc map' =
    let printRow (k, v) = "  " <> prettyDoc k <> ":" <+> prettyDoc v
     in encloseSep lbrace rbrace comma (printRow <$> M.toList map')

instance (Printer a, Printer b) => Printer (a, b) where
  prettyDoc (a, b) = tupled [prettyDoc a, prettyDoc b]

instance (Printer a, Printer b, Printer c) => Printer (a, b, c) where
  prettyDoc (a, b, c) =
    tupled [prettyDoc a, prettyDoc b, prettyDoc c]

instance (Printer a, Printer b, Printer c, Printer d) => Printer (a, b, c, d) where
  prettyDoc (a, b, c, d) =
    tupled
      [prettyDoc a, prettyDoc b, prettyDoc c, prettyDoc d]

instance (Printer a) => Printer (Set a) where
  prettyDoc as = list (prettyDoc <$> S.toList as)

instance (Printer a) => Printer (NES.NESet a) where
  prettyDoc as = prettyDoc (NES.toSet as)
