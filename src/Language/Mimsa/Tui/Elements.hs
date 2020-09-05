{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Tui.Elements where

import Brick
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as B
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.List as L
import qualified Data.Text as T
import Language.Mimsa.Printer
import Language.Mimsa.Tui.Styles

-- file for generic reusable UI pieces that don't know about our state

listDrawElement :: (Show a) => Bool -> a -> Widget ()
listDrawElement sel a =
  let selStr s =
        if sel
          then withAttr customAttr (str s)
          else str s
   in C.hCenter (selStr $ show a)

drawListWithTitle :: (Show a) => String -> L.List () a -> Widget ()
drawListWithTitle title l = ui
  where
    label = withAttr "title" $ str title
    ui =
      B.borderWithLabel label
        $ vLimit 30
        $ hLimit 50
        $ L.renderList listDrawElement True l

centeredBox :: [Widget a] -> Widget a
centeredBox items = C.vCenter $ vBox (C.hCenter <$> items)

drawInfo :: String -> Widget ()
drawInfo = str

drawPretty :: (Printer a) => a -> Widget ()
drawPretty = str . T.unpack . prettyPrint

twoColumnLayout :: String -> Widget a -> Widget a -> Widget a
twoColumnLayout title left right =
  Brick.withBorderStyle B.unicode $
    B.borderWithLabel
      (withAttr "title" $ str title)
      (centeredBox [left] <+> B.vBorder <+> centeredBox [right])
