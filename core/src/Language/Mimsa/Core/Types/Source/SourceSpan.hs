{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Language.Mimsa.Core.Types.Source.SourceSpan (SourceSpan (..), sourceSpan) where

import qualified Data.Aeson as JSON
import Data.OpenApi
import GHC.Generics
import Data.Text (Text)
import qualified Data.Text as T
import Language.Mimsa.Core.Types.AST.Annotation

data SourceSpan = SourceSpan
  { ssRowStart :: Int,
    ssRowEnd :: Int,
    ssColStart :: Int,
    ssColEnd :: Int
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (JSON.ToJSON, ToSchema)

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
              ssColStart = startCol + 1,
              ssColEnd = endCol + 1
            }
        )
sourceSpan _ _ = Nothing
