{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Printer
  ( Printer (..),
    renderWithWidth,
  )
where

-- the Printer type class is used for internal debugging
-- prettyPrint is for debug output
-- prettyDoc returns a Prettyprinter doc for nicer output

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Prettyprinter
import Prettyprinter.Render.Text

renderWithWidth :: Int -> Doc ann -> Text
renderWithWidth w doc = renderStrict (layoutPretty layoutOptions (unAnnotate doc))
  where
    layoutOptions = LayoutOptions {layoutPageWidth = AvailablePerLine w 1}

class Printer a where
  prettyPrint :: a -> Text
  default prettyPrint :: a -> Text
  prettyPrint = renderWithWidth 40 . prettyDoc

  prettyDoc :: a -> Doc ann
  default prettyDoc :: a -> Doc ann
  prettyDoc = pretty . T.unpack . prettyPrint

instance (Printer e, Printer a) => Printer (Either e a) where
  prettyDoc (Left e) = prettyDoc e
  prettyDoc (Right a) = prettyDoc a

instance Printer () where
  prettyDoc = const ""

instance (Printer a) => Printer (Maybe a) where
  prettyDoc (Just a) = prettyDoc a
  prettyDoc _ = mempty

instance Printer Text where
  prettyPrint a = a
  prettyDoc = pretty

instance Printer Bool where
  prettyPrint = T.pack . show

instance Printer Int where
  prettyPrint = T.pack . show

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
