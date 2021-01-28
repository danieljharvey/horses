{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Repl.Types where

import Control.Monad.Except
import Control.Monad.Reader
import qualified Data.Text.IO as T
import Language.Mimsa.Printer
import Language.Mimsa.Server.EnvVars
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.Typechecker

type ReplM ann = ReaderT MimsaConfig (ExceptT (Error ann) IO)

runReplM ::
  (Printer ann, Show ann) =>
  MimsaConfig ->
  ReplM ann a ->
  IO (Maybe a)
runReplM cfg computation = do
  either' <- runExceptT (runReaderT computation cfg)
  case either' of
    Right a -> pure (Just a)
    Left a -> do
      T.putStrLn (prettyPrint a)
      pure Nothing

replPrint :: (Printer a) => a -> ReplM ann ()
replPrint a = liftIO $ T.putStrLn (prettyPrint a)

liftRepl :: Either (Error ann) a -> ReplM ann a
liftRepl (Right a) = pure a
liftRepl (Left e) = throwError e

liftExceptTToRepl :: (e -> Error ann) -> ExceptT e IO a -> ReplM ann a
liftExceptTToRepl errorF comp = do
  value <- liftIO $ runExceptT comp
  case value of
    Right a -> pure a
    Left e -> throwError (errorF e)

data ReplAction ann
  = Help
  | Info (Expr Name ann)
  | Evaluate (Expr Name ann)
  | Tree (Expr Name ann)
  | Bind Name (Expr Name ann)
  | OutputJS (Expr Name ann)
  | TypeSearch MonoType
  | BindType DataType
  | Versions Name
  | ListBindings
  | AddUnitTest TestName (Expr Name ann)
  | ListTests (Maybe Name)
