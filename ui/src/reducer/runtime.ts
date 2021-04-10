import { State, Action, Event } from './types'
import { EventReducerRuntime } from '../utils/useEventReducer'
import {
  evaluate,
  createProject,
  getExpression,
  bindExpression,
  listBindings,
  addUnitTest,
} from '../service/project'
import { ExprHash } from '../types/'
import * as E from 'fp-ts/Either'
import { setScreen } from './view/reducer'
import { projectSet } from './project/helpers'
import { log } from './console/reducer'
import { emptyEditor } from './editor/helpers'
import * as H from 'history'
import { storeProjectData } from './project/reducer'

export const runtime = (
  history: H.History
): EventReducerRuntime<State, Action, Event> => (
  dispatch,
  state,
  event
) => {
  switch (event.type) {
    case 'SaveToSessionStorage':
      projectSet({ hash: event.projectHash })
      history.push(`/project/${event.projectHash}`)
      dispatch(
        log(
          `Saved ${event.projectHash} to session storage`,
          Date.now()
        )
      )

      return
    case 'ListBindings':
      dispatch(log('Saving to session storage', Date.now()))
      listBindings({
        lbProjectHash: event.projectHash,
      }).then(data => {
        dispatch(storeProjectData(data.lbProjectData))
      })
      return
    case 'CreateProject':
      createProject().then(data => {
        dispatch(storeProjectData(data.cpProjectData))

        dispatch(
          setScreen({
            type: 'scratch',
            editor: emptyEditor,
          })
        )
      })
      return
    case 'FetchExpressions':
      const fetchAndDispatch = (exprHash: ExprHash) =>
        getExpression({
          geExprHash: exprHash,
          geProjectHash: event.projectHash,
        }).then(a =>
          dispatch({
            type: 'FetchExpressionSuccess',
            exprHash,
            storeExpression: a.geExpressionData,
          })
        )
      const hashes = event.hashes.filter(
        exprHash =>
          !Object.keys(state.project.store).includes(
            exprHash
          )
      )
      hashes.forEach(fetchAndDispatch)
      return
    case 'EvaluateExpression':
      evaluate({
        erCode: event.code,
        erProjectHash: state.project.projectHash,
      })
        .then(a => {
          if (E.isRight(a)) {
            dispatch({
              type: 'EvaluateExpressionSuccess',
              expression: a.right,
            })
          } else {
            dispatch({
              type: 'EvaluateExpressionFailure',
              typeError: a.left,
            })
          }
        })
        .catch(_ =>
          dispatch({
            type: 'EvaluateExpressionError',
          })
        )
      return
    case 'BindExpression':
      bindExpression({
        beProjectHash: state.project.projectHash,
        beExpression: event.code,
        beBindingName: event.bindingName,
      })
        .then(res => {
          if (E.isRight(res)) {
            dispatch({
              type: 'BindExpressionSuccess',
              expression: res.right.beExpressionData,
              bindingName: event.bindingName,
              updatedTestsCount:
                res.right.beUpdatedTestsCount,
            })
            dispatch(
              storeProjectData(res.right.beProjectData)
            )
          } else {
            dispatch({
              type: 'BindExpressionFailure',
              error: res.left,
            })
          }
        })
        .catch(_ =>
          dispatch({
            type: 'BindExpressionFailure',
            error: 'Could not bind expression',
          })
        )
      return
    case 'AddUnitTest':
      addUnitTest({
        autProjectHash: state.project.projectHash,
        autExpression: event.code,
        autTestName: event.testName,
      })
        .then(res => {
          if (E.isRight(res)) {
            dispatch({
              type: 'AddUnitTestSuccess',
              unitTest: res.right.autUnitTest,
            })
            dispatch(
              storeProjectData(res.right.autProjectData)
            )
          } else {
            dispatch({
              type: 'AddUnitTestFailure',
              error: res.left,
            })
          }
        })
        .catch(_ =>
          dispatch({
            type: 'BindExpressionFailure',
            error: 'Could not bind expression',
          })
        )
  }
}
