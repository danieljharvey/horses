{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Repl.Types where

import Control.Monad.Except
import qualified Data.Text.IO as T
import Language.Mimsa.Printer
import Language.Mimsa.Types

type ReplM = ExceptT Error IO

runReplM :: ReplM a -> IO (Maybe a)
runReplM computation = do
  either' <- runExceptT computation
  case either' of
    Right a -> pure (Just a)
    Left a -> do
      T.putStrLn (prettyPrint a)
      pure Nothing

replPrint :: (Printer a) => a -> ReplM ()
replPrint a = liftIO $ T.putStrLn (prettyPrint a)

liftRepl :: Either Error a -> ReplM a
liftRepl (Right a) = pure a
liftRepl (Left e) = throwError e

data ReplAction
  = Help
  | Info (Expr Name)
  | Evaluate (Expr Name)
  | Tree (Expr Name)
  | Bind Name (Expr Name)
  | Versions Name
  | ListBindings
  | Watch
  | Tui
