import { State, Action, Event } from './types'
import { EventReducerRuntime } from '../hooks/useEventReducer'
import {
  evaluate,
  bindExpression,
  listBindings,
} from '../service/project'
import { getModule } from '../service/module'

import { ModuleHash } from '../types/'
import { setScreen } from './view/actions'
import { projectSet } from './project/helpers'
import { log } from './console/actions'
import * as H from 'history'
import {
  fetchModuleSuccess,
  storeProjectData,
} from './project/actions'
import * as T from 'fp-ts/Task'
import { pipe } from 'fp-ts/function'
import * as TE from 'fp-ts/TaskEither'
import {
  evaluateExpressionFailure,
  evaluateExpressionSuccess,
  bindExpressionSuccess,
  bindExpressionFailure,
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

      case 'FetchModule':
        const fetchModuleAndDispatch = (
          moduleHash: ModuleHash
        ) =>
          pipe(
            getModule(moduleHash),
            TE.map(fetchModuleSuccess)
          )

        return pipe(
          fetchModuleAndDispatch(event.moduleHash),
          TE.fold(
            (_) => T.of([]),
            (action) => T.of([action] as Action[])
          )
        )

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
                    event.bindingName
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
    }
  }
