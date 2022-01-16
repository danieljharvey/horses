import { State, Action, Event } from './types'
import { EventReducerRuntime } from '../utils/useEventReducer'
import {
  evaluate,
  createProject,
  getExpression,
  bindExpression,
  listBindings,
  addUnitTest,
  upgradeExpression,
} from '../service/project'
import { ExprHash } from '../types/'
import { setScreen } from './view/reducer'
import { projectSet } from './project/helpers'
import { log } from './console/reducer'
import { emptyEditor } from './editor/helpers'
import * as H from 'history'
import { storeProjectData } from './project/reducer'
import * as T from 'fp-ts/Task'
import { pipe } from 'fp-ts/function'
import * as TE from 'fp-ts/TaskEither'

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
            getExpression({
              geExprHash: exprHash,
              geProjectHash: event.projectHash,
            }),
            TE.map((a) => ({
              type: 'FetchExpressionSuccess' as const,
              exprHash,
              storeExpression: a.geExpressionData,
            }))
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
      case 'EvaluateExpression':
        return pipe(
          evaluate({
            erCode: event.code,
            erProjectHash: state.project.projectHash,
          }),
          TE.bimap(
            (e) => [
              {
                type: 'EvaluateExpressionFailure' as const,
                typeError: e,
              },
            ],
            (a) => [
              {
                type: 'EvaluateExpressionSuccess' as const,
                expression: a,
              },
            ]
          ),
          flatten()
        )
      case 'BindExpression':
        return pipe(
          bindExpression({
            beProjectHash: state.project.projectHash,
            beExpression: event.code,
            beBindingName: event.bindingName,
          }),
          TE.bimap(
            (e) => [
              {
                type: 'BindExpressionFailure' as const,
                error: e,
              },
            ],
            (a) => [
              {
                type: 'BindExpressionSuccess' as const,
                expression: a.beExpressionData,
                bindingName: event.bindingName,
                tests: a.beTestData,
              },
              storeProjectData(a.beProjectData),
            ]
          ),
          flatten()
        )
      case 'AddUnitTest':
        return pipe(
          addUnitTest({
            autProjectHash: state.project.projectHash,
            autExpression: event.code,
            autTestName: event.testName,
          }),
          TE.bimap(
            (e) => [
              {
                type: 'AddUnitTestFailure' as const,
                error: e,
              },
            ],
            (a) => [
              {
                type: 'AddUnitTestSuccess' as const,
                tests: a.autTestResult,
              },
              storeProjectData(a.autProjectData),
            ]
          ),
          flatten()
        )
      case 'UpgradeExpression':
        return pipe(
          upgradeExpression({
            upProjectHash: state.project.projectHash,
            upBindingName: event.bindingName,
          }),
          TE.bimap(
            (e) => [
              {
                type: 'UpgradeExpressionFailure' as const,
                error: e,
              },
            ],
            ({ upExpressionData, upProjectData }) => [
              {
                type: 'UpgradeExpressionSuccess' as const,
                tests: {
                  tdUnitTests: [],
                  tdPropertyTests: [],
                },
                expression: upExpressionData,
                bindingName: event.bindingName,
              },
              storeProjectData(upProjectData),
            ]
          ),
          flatten()
        )
    }
  }
