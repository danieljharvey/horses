{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Smol.Backend.Compile.RunLLVM (run, RunResult (..)) where

import Control.Exception (bracket)
import Data.FileEmbed
import Data.String.Conversions
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import LLVM.AST hiding (Type, function)
import LLVM.Pretty
import System.CPUTime
import System.Directory
import System.IO
import System.Posix.Temp
import qualified System.Process as Sys
import qualified Text.Printf as Printf

-- these are saved in a file that is included in compilation
cRuntime :: Text
cRuntime =
  T.decodeUtf8 $(makeRelativeToProject "static/runtime.c" >>= embedFile)

time :: IO t -> IO (Text, t)
time a = do
  start <- getCPUTime
  v <- a
  end <- getCPUTime
  let diff = fromIntegral (end - start) / (10 ^ (12 :: Integer))
  let formatted = Printf.printf "%0.9f sec" (diff :: Double)
  return (T.pack formatted, v)

-- compile some shit
compile :: Module -> FilePath -> IO ()
compile llvmModule outfile =
  bracket (mkdtemp "build") removePathForcibly $ \buildDir ->
    withCurrentDirectory buildDir $ do
      -- create temporary file for "output.ll"
      (llvm, llvmHandle) <- mkstemps "output" ".ll"
      (runtime, runtimeHandle) <- mkstemps "runtime" ".c"

      let moduleText = cs (ppllvm llvmModule)

      T.putStrLn moduleText

      -- write the llvmmodule Smol.Backend.to a file
      T.hPutStrLn llvmHandle moduleText
      T.hPutStrLn runtimeHandle cRuntime

      hClose llvmHandle
      hClose runtimeHandle
      -- link the runtime with the assembly
      Sys.callProcess
        "clang"
        ["-Wno-override-module", "-lm", llvm, runtime, "-o", "../" <> outfile]

data RunResult = RunResult
  { rrResult :: Text,
    rrComptime :: Text,
    rrRuntime :: Text
  }

-- run the code, get the output, die
run :: [(String, String)] -> Module -> IO RunResult
run runArgs llvmModule = do
  (compTime, _) <- time (compile llvmModule "./a.out")
  let process =
        (Sys.proc "./a.out" [])
          { Sys.env = Just runArgs
          }
  (runTime, result) <- time (cs <$> Sys.readCreateProcess process "")
  removePathForcibly "./a.out"
  pure (RunResult result compTime runTime)
