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
import Language.Mimsa.Monad
import Language.Mimsa.Printer
import Language.Mimsa.Project.Versions
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Project

doVersions :: Project Annotation -> Name -> MimsaM (Error Annotation) ()
doVersions env name = do
  versions <- mimsaFromEither $ findVersions env name
  let showIt (i, mt, expr', usages, _) = do
        replOutput $
          "#" <> T.pack (show i)
            <> ( if NE.length versions == i
                   then " (current)"
                   else ""
               )
        replOutput $ prettyPrint (expr', mt)
        if S.null usages
          then replOutput ("Dependency of 0 functions" :: Text)
          else
            replOutput $
              "Dependency of " <> (T.pack . show . S.size) usages
                <> " functions"
   in traverse_ showIt versions
