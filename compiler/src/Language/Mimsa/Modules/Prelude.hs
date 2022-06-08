{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Mimsa.Modules.Prelude ( maybeInput, preludeInput, 
      stateInput, parserInput, nonEmptyArrayInput, arrayInput) where

import Data.FileEmbed
import Data.Text (Text)
import qualified Data.Text.Encoding as T

maybeInput :: Text
maybeInput = T.decodeUtf8 $(embedFile "static/modules/Maybe.mimsa")

stateInput :: Text
stateInput = T.decodeUtf8 $(embedFile "static/modules/State.mimsa")

preludeInput :: Text
preludeInput = T.decodeUtf8 $(embedFile "static/modules/Prelude.mimsa")

parserInput :: Text
parserInput = T.decodeUtf8 $(embedFile "static/modules/Parser.mimsa")

arrayInput :: Text
arrayInput = T.decodeUtf8 $(embedFile "static/modules/Array.mimsa")

nonEmptyArrayInput :: Text
nonEmptyArrayInput = T.decodeUtf8 $(embedFile "static/modules/NonEmptyArray.mimsa")
