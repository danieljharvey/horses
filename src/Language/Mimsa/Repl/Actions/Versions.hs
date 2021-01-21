{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Repl.Actions.Versions
  ( doVersions,
  )
where

import Data.Foldable (traverse_)
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Language.Mimsa.Project.Versions
import Language.Mimsa.Repl.Types
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Project

doVersions :: Project Annotation -> Name -> ReplM Annotation ()
doVersions env name = do
  versions <- liftRepl $ findVersions env name
  let showIt (i, mt, expr', usages) = do
        replPrint $
          "#" <> T.pack (show i)
            <> ( if NE.length versions == i
                   then " (current)"
                   else ""
               )
        replPrint (expr', mt)
        if S.null usages
          then replPrint ("Dependency of 0 functions" :: Text)
          else
            replPrint $
              "Dependency of " <> (T.pack . show . S.size) usages
                <> " functions"
   in traverse_ showIt versions
