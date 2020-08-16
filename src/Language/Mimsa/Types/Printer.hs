{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types.Printer where

import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T

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
  prettyPrint (a, b) = T.intercalate "\n" [prettyPrint a, prettyPrint b]

instance (Printer a, Printer b, Printer c) => Printer (a, b, c) where
  prettyPrint (a, b, c) =
    T.intercalate
      "\n"
      [prettyPrint a, prettyPrint b, prettyPrint c]

instance (Printer a, Printer b, Printer c, Printer d) => Printer (a, b, c, d) where
  prettyPrint (a, b, c, d) =
    T.intercalate
      "\n"
      [prettyPrint a, prettyPrint b, prettyPrint c, prettyPrint d]

instance (Printer a) => Printer (Set a) where
  prettyPrint as = "[" <> T.intercalate ", " (prettyPrint <$> S.toList as) <> "]"
