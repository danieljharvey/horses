{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS -Wno-orphans #-}

module Calc.Repl
  ( repl,
  )
where

import qualified Calc.Compile.RunLLVM as Run
import Calc.Compile.ToLLVM
import Calc.Parser
import Calc.Parser.Types
import Calc.Typecheck.Elaborate
import Calc.Typecheck.Error
import Control.Monad.IO.Class
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import qualified Error.Diagnose as Diag
import Error.Diagnose.Compat.Megaparsec
import System.Console.Haskeline

instance HasHints Void msg where
  hints _ = mempty

repl :: IO ()
repl = do
  putStrLn "Welcome to llvm-calc"
  putStrLn "Exit with :quit"
  runInputT defaultSettings loop
  where
    loop :: InputT IO ()
    loop = do
      minput <- getInputLine ":> "
      case minput of
        Nothing -> return ()
        Just ":quit" -> return ()
        Just input -> do
          case parseModule (T.pack input) of
            Left bundle -> do
              printDiagnostic (fromErrorBundle bundle input)
              loop
            Right expr -> case elaborateModule expr of
              Left typeErr -> do
                printDiagnostic (typeErrorDiagnostic (T.pack input) typeErr)
                loop
              Right typedExpr -> do
                resp <- liftIO $ fmap Run.rrResult (Run.run (moduleToLLVM typedExpr))
                liftIO $ putStrLn (T.unpack resp)
                loop

printDiagnostic :: (MonadIO m) => Diag.Diagnostic Text -> m ()
printDiagnostic =
  Diag.printDiagnostic
    Diag.stderr
    True
    True
    4
    Diag.defaultStyle

-- | turn Megaparsec error + input into a Diagnostic
fromErrorBundle :: ParseErrorType -> String -> Diag.Diagnostic Text
fromErrorBundle bundle input =
  let diag =
        errorDiagnosticFromBundle
          Nothing
          "Parse error on input"
          Nothing
          bundle
   in Diag.addFile diag replFilename input
