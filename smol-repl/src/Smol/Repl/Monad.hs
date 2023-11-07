module Smol.Repl.Monad (ReplEnv(..)) where

import OpenTelemetry.Trace

data ReplEnv = ReplEnv {
  tracer :: Tracer
                       }
