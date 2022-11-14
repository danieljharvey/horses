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
    treeInput,
  )
where

import Data.FileEmbed
import Data.Text (Text)
import qualified Data.Text.Encoding as T

maybeInput :: Text
maybeInput = T.decodeUtf8 $(makeRelativeToProject "static/modules/Maybe.mimsa" >>= embedFile)

stateInput :: Text
stateInput = T.decodeUtf8 $(makeRelativeToProject "static/modules/State.mimsa" >>= embedFile)

preludeInput :: Text
preludeInput = T.decodeUtf8 $(makeRelativeToProject "static/modules/Prelude.mimsa" >>= embedFile)

parserInput :: Text
parserInput = T.decodeUtf8 $(makeRelativeToProject "static/modules/Parser.mimsa" >>= embedFile)

arrayInput :: Text
arrayInput = T.decodeUtf8 $(makeRelativeToProject "static/modules/Array.mimsa" >>= embedFile)

nonEmptyArrayInput :: Text
nonEmptyArrayInput = T.decodeUtf8 $(makeRelativeToProject "static/modules/NonEmptyArray.mimsa" >>= embedFile)

stringInput :: Text
stringInput = T.decodeUtf8 $(makeRelativeToProject "static/modules/String.mimsa" >>= embedFile)

monoidInput :: Text
monoidInput = T.decodeUtf8 $(makeRelativeToProject "static/modules/Monoid.mimsa" >>= embedFile)

readerInput :: Text
readerInput = T.decodeUtf8 $(makeRelativeToProject "static/modules/Reader.mimsa" >>= embedFile)

eitherInput :: Text
eitherInput = T.decodeUtf8 $(makeRelativeToProject "static/modules/Either.mimsa" >>= embedFile)

theseInput :: Text
theseInput = T.decodeUtf8 $(makeRelativeToProject "static/modules/These.mimsa" >>= embedFile)

treeInput :: Text
treeInput = T.decodeUtf8 $(makeRelativeToProject "static/modules/Tree.mimsa" >>= embedFile)
