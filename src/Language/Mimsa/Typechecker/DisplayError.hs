{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Typechecker.DisplayError
  ( displayError,
  )
where

import qualified Data.List.NonEmpty as NE
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Language.Mimsa.Types.Error.TypeError
import Text.Megaparsec

displayError :: Text -> TypeError -> Text
displayError input typeError =
  T.pack $ errorBundlePretty $ createErrorBundle input typeError

toFancy :: TypeError -> ParseError Text TypeError
toFancy typeErr =
  let (start, _) = getErrorPos typeErr
   in FancyError start (S.singleton (ErrorCustom typeErr))

createErrorBundle :: Text -> TypeError -> ParseErrorBundle Text TypeError
createErrorBundle input typeError =
  let initialState =
        PosState
          { pstateInput = input,
            pstateOffset = 0,
            pstateSourcePos = initialPos "repl",
            pstateTabWidth = defaultTabWidth,
            pstateLinePrefix = ""
          }
   in ParseErrorBundle
        { bundleErrors = NE.fromList [toFancy typeError],
          bundlePosState = initialState
        }
