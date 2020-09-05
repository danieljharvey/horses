{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Printer where

-- all of the pretty printing will happen here
-- rather than in the types files
-- to keep those smaller

import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text

renderWithWidth :: Int -> Doc ann -> Text
renderWithWidth w doc = renderStrict (layoutPretty layoutOptions (unAnnotate doc))
  where
    layoutOptions = LayoutOptions {layoutPageWidth = AvailablePerLine w 1}

class Printer a where
  prettyPrint :: a -> Text
  default prettyPrint :: (Show a) => a -> Text
  prettyPrint = T.pack . show

instance Printer Text where
  prettyPrint a = a

instance Printer Int where
  prettyPrint = T.pack . show

instance (Printer a) => Printer [a] where
  prettyPrint as = T.intercalate ", " (prettyPrint <$> as)

instance (Printer a, Printer b) => Printer (a, b) where
  prettyPrint (a, b) = "\n " <> T.intercalate "\n " [prettyPrint a, prettyPrint b]

instance (Printer k, Printer v) => Printer (Map k v) where
  prettyPrint map' =
    let printRow (k, v) = prettyPrint k <> ": " <> prettyPrint v
     in T.intercalate ", " (printRow <$> M.toList map')

instance (Printer a, Printer b, Printer c) => Printer (a, b, c) where
  prettyPrint (a, b, c) =
    "\n "
      <> T.intercalate
        "\n "
        [prettyPrint a, prettyPrint b, prettyPrint c]

instance (Printer a, Printer b, Printer c, Printer d) => Printer (a, b, c, d) where
  prettyPrint (a, b, c, d) =
    "\n "
      <> T.intercalate
        "\n "
        [prettyPrint a, prettyPrint b, prettyPrint c, prettyPrint d]

instance (Printer a) => Printer (Set a) where
  prettyPrint as = "[" <> T.intercalate ", " (prettyPrint <$> S.toList as) <> "]"
