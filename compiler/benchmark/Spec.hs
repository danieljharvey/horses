module Main
  ( main,
  )
where

import Criterion.Main
import qualified Language.Mimsa.Actions.Monad as Actions
import Language.Mimsa.Project.Stdlib
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Project

-- build the stdlib, exploding if it fails (so that doesn't look like an
-- incredible speedup)
buildThing ::
  Actions.ActionM () ->
  Project Annotation ->
  Project Annotation
buildThing action prj =
  case Actions.run prj action of
    Right (proj, _, _) -> proj
    Left e -> error (show e)

arrayFns' :: Actions.ActionM ()
arrayFns' = do
  baseFns
  arrayFns

nonEmptyArrayFns' :: Actions.ActionM ()
nonEmptyArrayFns' = do
  arrayFns'
  nonEmptyArrayFns

-- Our benchmark harness.
main :: IO ()
main =
  defaultMain
    [ bgroup
        "build stdlib"
        [ bench "baseFns" $ whnf (buildThing baseFns) mempty,
          bench "arrayFns" $ whnf (buildThing arrayFns') mempty,
          bench "nonEmptyArrayFns" $ whnf (buildThing nonEmptyArrayFns') mempty,
          bench "allFns" $ whnf (buildThing allFns) mempty
        ]
    ]
