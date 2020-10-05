{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Repl.ExpressionWatch
  ( doWatch,
  )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Aeson as JSON
import Data.Bifunctor (first)
import Data.IORef
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Language.Mimsa.Actions
import Language.Mimsa.Parser (parseExpr)
import Language.Mimsa.Printer
import Language.Mimsa.Project.Versions
import Language.Mimsa.Repl.ExpressionBind
import Language.Mimsa.Repl.Types
import Language.Mimsa.Repl.Watcher
import Language.Mimsa.Types

---------

scratchFilename :: String
scratchFilename = "scratch.mimsa"

writeExpression :: (MonadIO m) => Project ann -> Name -> m ()
writeExpression env name =
  case findStoreExpressionByName env name of
    Just storeExpr ->
      liftIO $
        T.writeFile
          ("./" <> scratchFilename)
          (prettyPrint (storeExpression storeExpr))
    Nothing -> pure ()

doWatch ::
  ( Eq ann,
    Monoid ann,
    Show ann,
    Printer ann,
    JSON.ToJSON ann
  ) =>
  Project ann ->
  Name ->
  ReplM ann (Project ann)
doWatch env name = do
  writeExpression env name
  ioEnv <- liftIO $ newIORef env
  _ <-
    liftIO $
      watchFile
        "./"
        ( do
            env' <- liftIO $ readIORef ioEnv
            newEnv <- runReplM (onFileChange env' name)
            case newEnv of
              Just newEnv' -> liftIO $ writeIORef ioEnv newEnv'
              Nothing -> pure ()
        )
  liftIO $ readIORef ioEnv

onFileChange ::
  (Eq ann, Monoid ann, JSON.ToJSON ann) =>
  Project ann ->
  Name ->
  ReplM ann (Project ann)
onFileChange env name = do
  text <-
    liftIO $ T.readFile $ "./" <> scratchFilename
  replPrint (T.pack scratchFilename <> " updated!")
  expr <-
    liftRepl $ first ParseErr (parseExpr (T.strip text))
  (ResolvedExpression type' storeExpr _ _ _) <-
    liftRepl $ getTypecheckedStoreExpression env expr
  replPrint $
    "+ Using the following from scope: "
      <> prettyPrint (storeBindings storeExpr)
  replPrint $
    "Bound " <> prettyPrint name <> " to " <> prettyPrint expr
      <> " :: "
      <> prettyPrint type'
  bindStoreExpression env storeExpr name
