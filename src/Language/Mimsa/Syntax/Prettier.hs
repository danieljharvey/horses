{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Syntax.Prettier
  ( renderWithWidth,
  )
where

import Data.Text (Text)
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text

renderWithWidth :: Int -> Doc ann -> Text
renderWithWidth w doc = renderStrict (layoutPretty layoutOptions (unAnnotate doc))
  where
    layoutOptions = LayoutOptions {layoutPageWidth = AvailablePerLine w 1}
