{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Repl.Types where

import Control.Monad.Except
import qualified Data.Text.IO as T
import Language.Mimsa.Printer
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers

type ReplM a = ExceptT (Error a) IO

runReplM :: (Show ann) => ReplM ann a -> IO (Maybe a)
runReplM computation = do
  either' <- runExceptT computation
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

data ReplAction a
  = Help
  | Info (Expr a Name)
  | Evaluate (Expr a Name)
  | Tree (Expr a Name)
  | Bind Name (Expr a Name)
  | OutputJS (Expr a Name)
  | BindType DataType
  | Versions Name
  | ListBindings
  | Watch Name
  | Tui
