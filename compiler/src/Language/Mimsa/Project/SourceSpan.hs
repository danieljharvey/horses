module Language.Mimsa.Project.SourceSpan where

import Data.Text (Text)
import qualified Data.Text as T
import Language.Mimsa.Types.AST.Annotation
import Language.Mimsa.Types.Project.SourceSpan

lineLengths :: Text -> [Int]
lineLengths tx = T.length <$> T.lines tx

toColumnAndRow :: [Int] -> Int -> (Int, Int)
toColumnAndRow = go 1
  where
    go row [] col =
      (row, col)
    go row (line : rest) col
      | (col - 1) >= line =
        go (row + 1) rest (col - line - 1)
    go row _ col = (row, col)

sourceSpan :: Text -> Annotation -> Maybe SourceSpan
sourceSpan tx (Location start end) =
  let (startRow, startCol) =
        toColumnAndRow (lineLengths tx) start
      (endRow, endCol) =
        toColumnAndRow (lineLengths tx) end
   in Just
        ( SourceSpan
            { ssRowStart = startRow,
              ssRowEnd = endRow,
              ssColStart = startCol,
              ssColEnd = endCol
            }
        )
sourceSpan _ _ = Nothing
