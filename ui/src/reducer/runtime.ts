import { State, Action, Event } from './types'
import { EventReducerRuntime } from '../hooks/useEventReducer'
import {
  evaluate,
  bindExpression,
  listBindings,
  addUnitTest,
  upgradeExpression,
  optimiseExpression,
} from '../service/project'
import { getExpressions } from '../service/expression'
import { ExprHash } from '../types/'
import { setScreen } from './view/actions'
import { projectSet } from './project/helpers'
import { log } from './console/actions'
import * as H from 'history'
import {
  fetchExpressionsSuccess,
  storeProjectData,
} from './project/actions'
import * as T from 'fp-ts/Task'
import { pipe } from 'fp-ts/function'
import * as TE from 'fp-ts/TaskEither'
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
  expressionPreviewSuccess,
} from './editor/actions'
import { editScreen } from './view/screen'
import * as O from 'fp-ts/Option'
import { currentEditorO } from './editor/selector'

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

      case 'FetchExpressions':
        const fetchAndDispatch = (exprHashes: ExprHash[]) =>
          pipe(
            exprHashes.length > 0
              ? getExpressions(exprHashes)
              : TE.right({ geExpressionsData: {} }),
            TE.map(({ geExpressionsData }) =>
              fetchExpressionsSuccess(geExpressionsData)
            )
          )
        const hashes = event.hashes.filter(
          (exprHash) =>
            !Object.keys(state.project.store).includes(
              exprHash
            )
        )
        const x = pipe(
          fetchAndDispatch(hashes),
          TE.fold(
            (_) => T.of([]),
            (action) => T.of([action] as Action[])
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
            (a): Action[] => {
              if (event.updateProject) {
                // if we can get the editor, redirect to it
                const gotoEditScreen = pipe(
                  currentEditorO.getOption(state),
                  O.match(
                    () => [],
                    (editorState) => [
                      setScreen(
                        editScreen(
                          event.bindingName,
                          editorState
                        )
                      ),
                    ]
                  )
                )
                return [
                  ...gotoEditScreen,
                  bindExpressionSuccess(
                    a.beExpressionData,
                    event.bindingName,
                    a.beTestData
                  ),
                  storeProjectData(a.beProjectData),
                ]
              }
              return [
                expressionPreviewSuccess(
                  a.beExpressionData
                ),
              ]
            }
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
