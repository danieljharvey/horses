import { Lens } from 'monocle-ts'
import * as O from 'fp-ts/Option'
import { UnitTestData, ExpressionData } from '../../types'
import {
    EventReducer,
    stateOnly,
    stateAndEvent,
} from '../../utils/useEventReducer'
import { ProjectEvent } from '../project/reducer'
import {
    EditorState,
    ExpressionResult,
    EditorAction,
    EditorEvent,
} from './types'
export * from './types'

export const editorNew = (): ExpressionResult => ({
    type: 'EditorNew',
})

export const showBinding = (
    expression: ExpressionData
): ExpressionResult => ({
    type: 'ShowBinding',
    expression,
})

export const showUpdatedBinding = (
    expression: ExpressionData,
    bindingName: string,
    updatedTestsCount: number
): ExpressionResult => ({
    type: 'ShowUpdatedBinding',
    expression,
    bindingName,
    updatedTestsCount,
})

export const showUnitTest = (
    unitTest: UnitTestData
): ExpressionResult => ({
    type: 'ShowUnitTest',
    unitTest,
})

const showError = (error: string): ExpressionResult => ({
    type: 'ShowError',
    error,
})

const evaluationError = (): ExpressionResult => ({
    type: 'EvaluationError',
})

const showEvaluate = (
    expression: ExpressionData,
    evaluatedValue: string
): ExpressionResult => ({
    type: 'ShowEvaluate',
    expression,
    evaluatedValue,
})

const staleL = Lens.fromProp<EditorState>()('stale')

export const editorReducer: EventReducer<
    EditorState,
    EditorAction,
    EditorEvent | ProjectEvent
> = (state, action) => {
    switch (action.type) {
        case 'UpdateCode':
            return stateOnly({
                ...state,
                code: action.text,
                stale: true,
            })
        case 'EvaluateExpression':
            return stateAndEvent(staleL.set(false)(state), {
                type: 'EvaluateExpression',
                code: state.code,
            })
        case 'EvaluateExpressionFailure':
            return stateOnly({
                ...state,
                expression: showError(action.typeError),
            })
        case 'EvaluateExpressionSuccess':
            return stateOnly({
                ...state,
                code: action.expression.erExpressionData.edPretty,
                expression: showEvaluate(
                    action.expression.erExpressionData,
                    action.expression.erResult
                ),
            })
        case 'EvaluateExpressionError':
            return stateOnly({
                ...state,
                expression: evaluationError(),
            })
        case 'BindExpression':
            return stateAndEvent(staleL.set(false)(state), {
                type: 'BindExpression',
                code: state.code,
                bindingName: action.bindingName,
            })
        case 'BindExpressionSuccess':
            return stateOnly({
                ...state,
                code: action.expression.edPretty,
                bindingName: O.some(action.bindingName),
                stale: false,
                expression: showUpdatedBinding(
                    action.expression,
                    action.bindingName,
                    action.updatedTestsCount
                ),
            })
        case 'BindExpressionFailure':
            return stateOnly({
                ...state,
                expression: showError(action.error),
            })
        case 'AddUnitTest':
            return stateAndEvent(staleL.set(false)(state), {
                type: 'AddUnitTest',
                testName: action.testName,
                code: state.code,
            })
        case 'AddUnitTestSuccess':
            return stateOnly({
                ...state,
                state: false,
                expression: showUnitTest(action.unitTest),
            })
        case 'AddUnitTestFailure':
            return stateOnly({
                ...state,
                expression: showError(action.error),
            })
        default:
            return stateOnly(state)
    }
}
