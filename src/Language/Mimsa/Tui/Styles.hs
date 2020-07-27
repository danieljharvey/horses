{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Tui.Styles where

import Brick
import qualified Brick.AttrMap as A
import qualified Brick.Widgets.List as L
import qualified Graphics.Vty as V

customAttr :: A.AttrName
customAttr = L.listSelectedAttr <> "custom"

highlight :: A.AttrName
highlight = "highlight"

titleText :: A.AttrName
titleText = "title"

stylesAttrMap :: A.AttrMap
stylesAttrMap =
  A.attrMap
    V.defAttr
    [ (L.listAttr, V.white `on` V.blue),
      (L.listSelectedAttr, V.blue `on` V.white),
      (customAttr, fg V.cyan),
      (highlight, fg V.cyan),
      (titleText, fg V.yellow)
    ]
