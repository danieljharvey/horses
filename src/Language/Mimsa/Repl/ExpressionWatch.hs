{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Repl.ExpressionWatch
  ( doWatch,
  )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bifunctor (first)
import Data.IORef
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Language.Mimsa.Actions
import Language.Mimsa.Parser (parseExprAndFormatError)
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
  Project Annotation ->
  Name ->
  ReplM Annotation (Project Annotation)
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
  Project Annotation ->
  Name ->
  ReplM Annotation (Project Annotation)
onFileChange env name = do
  text <-
    liftIO $ T.readFile $ "./" <> scratchFilename
  replPrint (T.pack scratchFilename <> " updated!")
  expr <-
    liftRepl $ first ParseErr (parseExprAndFormatError (T.strip text))
  (ResolvedExpression type' storeExpr _ _ _) <-
    liftRepl $ getTypecheckedStoreExpression env expr
  replPrint $
    "+ Using the following from scope: "
      <> prettyPrint (storeBindings storeExpr)
  replPrint $
    "Bound " <> prettyPrint name <> ".\n::\n" <> prettyPrint expr
      <> "\n\n"
      <> prettyPrint type'
  bindStoreExpression env storeExpr name
