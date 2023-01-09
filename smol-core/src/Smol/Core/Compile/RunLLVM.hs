{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Smol.Core.Compile.RunLLVM (run, moduleFromExpr, RunResult (..)) where

import Control.Exception (bracket)
import Data.FileEmbed
import Data.String.Conversions
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import Smol.Core.IR.FromExpr.Expr
import Smol.Core.IR.ToLLVM.ToLLVM
import LLVM.AST hiding (function)
import LLVM.Pretty
import System.CPUTime
import System.Directory
import System.IO
import System.Posix.Temp
import System.Process
import qualified Text.Printf as Printf
import qualified Smol.Core.Types as Smol

-- these are saved in a file that is included in compilation
cRuntime :: Text
cRuntime =
  T.decodeUtf8 $(makeRelativeToProject "static/runtime.c" >>= embedFile)

moduleFromExpr :: Smol.Expr (Smol.Type Smol.Annotation) -> Module
moduleFromExpr = irToLLVM . irFromExpr

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

      -- T.putStrLn moduleText

      -- write the llvmmodule Smol.Core.to a file
      T.hPutStrLn llvmHandle moduleText
      T.hPutStrLn runtimeHandle cRuntime

      hClose llvmHandle
      hClose runtimeHandle
      -- link the runtime with the assembly
      callProcess
        "clang"
        ["-Wno-override-module", "-lm", llvm, runtime, "-o", "../" <> outfile]

data RunResult = RunResult
  { rrResult :: Text,
    rrComptime :: Text,
    rrRuntime :: Text
  }

-- run the code, get the output, die
run :: Module -> IO RunResult 
run llvmModule = do 
  (compTime, _) <- time (compile llvmModule "./a.out")
  (runTime, result) <- time (cs <$> readProcess "./a.out" [] [])
  removePathForcibly "./a.out"
  pure (RunResult result compTime runTime)
