{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Mimsa.Modules.Prelude
  ( maybeInput,
    preludeInput,
    stateInput,
    parserInput,
    nonEmptyArrayInput,
    arrayInput,
    stringInput,
    monoidInput,
    readerInput,
    eitherInput,
    theseInput,
    treeInput
  )
where

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

stringInput :: Text
stringInput = T.decodeUtf8 $(embedFile "static/modules/String.mimsa")

monoidInput :: Text
monoidInput = T.decodeUtf8 $(embedFile "static/modules/Monoid.mimsa")

readerInput :: Text
readerInput = T.decodeUtf8 $(embedFile "static/modules/Reader.mimsa")

eitherInput :: Text
eitherInput = T.decodeUtf8 $(embedFile "static/modules/Either.mimsa")

theseInput :: Text
theseInput = T.decodeUtf8 $(embedFile "static/modules/These.mimsa")

treeInput :: Text
treeInput = T.decodeUtf8 $(embedFile "static/modules/Tree.mimsa")
