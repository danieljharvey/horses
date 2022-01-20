{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.ErrorFormat where

import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Language.Mimsa.Project.SourceSpan
import Language.Mimsa.Types.AST.Annotation
import Language.Mimsa.Types.Project.SourceItem
import Language.Mimsa.Types.Project.SourceSpan
import Language.Mimsa.Utils (mapWithIndex)

data Diagnostic = Diagnostic
  { diInput :: Text,
    diMessage :: Text,
    diSourceItems :: [SourceItem]
  }

toDiagnostic :: Text -> [(Text, Annotation)] -> Text -> Diagnostic
toDiagnostic message parts input =
  Diagnostic
    { diInput = input,
      diMessage = message,
      diSourceItems =
        catMaybes $
          ( \(label, ann) ->
              SourceItem label
                <$> sourceSpan input ann
          )
            <$> parts
    }

printDiagnostic :: Diagnostic -> Text
printDiagnostic (Diagnostic input msg items) =
  T.intercalate "\n" ([msg] <> (printSourceItem input <$> items))

getLines :: Int -> Int -> Text -> Text
getLines from to input =
  mconcat $
    catMaybes $
      mapWithIndex
        ( \i a ->
            if i >= from && i <= to
              then Just a
              else Nothing
        )
        (T.lines input)

getHighlight :: Int -> Int -> Text -> Text
getHighlight from to label =
  let min' = min from to - 1
      max' = max from to
      spaces = T.pack (replicate (min' - 1) ' ')
      arrows = T.pack (replicate (max' - min') '^')
   in spaces <> arrows <> "\n" <> spaces <> label

printSourceItem :: Text -> SourceItem -> Text
printSourceItem input (SourceItem label (SourceSpan rS rE cS cE)) =
  let code = getLines rS rE input
      highlight = getHighlight cS cE label
   in code <> "\n" <> highlight
