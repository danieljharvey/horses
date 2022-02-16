import { State, Action, Event } from './types'
import { EventReducerRuntime } from '../utils/useEventReducer'
import {
  evaluate,
  createProject,
  bindExpression,
  listBindings,
  addUnitTest,
  upgradeExpression,
  optimiseExpression,
} from '../service/project'
import { getExpression } from '../service/expression'
import { ExprHash } from '../types/'
import { setScreen } from './view/reducer'
import { projectSet } from './project/helpers'
import { log } from './console/reducer'
import { emptyEditor } from './editor/helpers'
import * as H from 'history'
import { storeProjectData } from './project/actions'
import * as T from 'fp-ts/Task'
import { pipe } from 'fp-ts/function'
import * as TE from 'fp-ts/TaskEither'
import { fetchExpressionSuccess } from './project/actions'
import {
  evaluateExpressionFailure,
  evaluateExpressionSuccess,
  addUnitTestFailure,
  addUnitTestSuccess,
  bindExpressionSuccess,
  bindExpressionFailure,
  upgradeExpressionSuccess,
  upgradeExpressionFailure,
  optimiseExpressionFailure,
  optimiseExpressionSuccess,
} from './editor/actions'

const orEmpty = <A>() =>
  TE.fold(
    () => T.of([]),
    (a: A[]) => T.of(a)
  )

const flatten = <E, A>() =>
  TE.fold(
    (e: E) => T.of<E | A>(e),
    (a: A) => T.of<E | A>(a)
  )

export const runtime =
  (
    history: H.History
  ): EventReducerRuntime<State, Action, Event> =>
  (state, event) => {
    switch (event.type) {
      case 'SaveToSessionStorage':
        projectSet({ hash: event.projectHash })
        history.push(`/project/${event.projectHash}`)
        return T.of([
          log(
            `Saved ${event.projectHash} to session storage`,
            Date.now()
          ),
        ])

      case 'ListBindings':
        return pipe(
          listBindings({
            lbProjectHash: event.projectHash,
          }),
          TE.map((data) => [
            storeProjectData(data.lbProjectData),
          ]),
          orEmpty()
        )
      case 'CreateProject':
        return pipe(
          createProject(),
          TE.map((data) => [
            storeProjectData(data.cpProjectData),
            setScreen({
              type: 'scratch' as const,
              editor: emptyEditor,
            }),
          ]),
          orEmpty()
        )
      case 'FetchExpressions':
        const fetchAndDispatch = (exprHash: ExprHash) =>
          pipe(
            getExpression(exprHash),
            TE.map((a) =>
              fetchExpressionSuccess(
                exprHash,
                a.geExpressionData
              )
            )
          )
        const hashes = event.hashes.filter(
          (exprHash) =>
            !Object.keys(state.project.store).includes(
              exprHash
            )
        )
        const x = pipe(
          TE.sequenceArray(hashes.map(fetchAndDispatch)),
          TE.fold(
            (_) => T.of([]),
            (actions) => T.of(actions as Action[])
          )
        )
        return x
      case 'DoEvaluateExpression':
        return pipe(
          evaluate({
            erCode: event.code,
            erProjectHash: state.project.projectHash,
          }),
          TE.bimap(
            (e) => [evaluateExpressionFailure(e)],
            (a) => [evaluateExpressionSuccess(a)]
          ),
          flatten()
        )
      case 'DoBindExpression':
        return pipe(
          bindExpression({
            beProjectHash: state.project.projectHash,
            beExpression: event.code,
            beBindingName: event.bindingName,
          }),
          TE.bimap(
            (e) => [bindExpressionFailure(e)],
            (a) => [
              bindExpressionSuccess(
                a.beExpressionData,
                event.bindingName,
                a.beTestData
              ),
              storeProjectData(a.beProjectData),
            ]
          ),
          flatten()
        )
      case 'DoAddUnitTest':
        return pipe(
          addUnitTest({
            autProjectHash: state.project.projectHash,
            autExpression: event.code,
            autTestName: event.testName,
          }),
          TE.bimap(
            (e) => [addUnitTestFailure(e)],
            (a) => [
              addUnitTestSuccess(a.autTestResult),
              storeProjectData(a.autProjectData),
            ]
          ),
          flatten()
        )
      case 'DoUpgradeExpression':
        return pipe(
          upgradeExpression({
            upProjectHash: state.project.projectHash,
            upBindingName: event.bindingName,
          }),
          TE.bimap(
            (e) => [upgradeExpressionFailure(e)],
            ({
              upExpressionData,
              upProjectData,
              upTestData,
            }) => [
              upgradeExpressionSuccess(
                upExpressionData,
                event.bindingName,
                upTestData
              ),
              storeProjectData(upProjectData),
            ]
          ),
          flatten()
        )
      case 'DoOptimiseExpression':
        return pipe(
          optimiseExpression({
            opProjectHash: state.project.projectHash,
            opBindingName: event.bindingName,
          }),
          TE.bimap(
            (e) => [optimiseExpressionFailure(e)],
            ({
              opExpressionData,
              opProjectData,
              opTestData,
            }) => [
              optimiseExpressionSuccess(
                opExpressionData,
                event.bindingName,
                opTestData
              ),
              storeProjectData(opProjectData),
            ]
          ),
          flatten()
        )
    }
  }
