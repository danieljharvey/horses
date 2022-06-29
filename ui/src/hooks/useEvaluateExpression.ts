import * as React from 'react'
import { evaluateModule } from '../service/project'
import {
  EvaluateModuleResponse,
  UserErrorResponse,
} from '../generated'
import { pipe } from 'fp-ts/function'
import * as E from 'fp-ts/Either'
import {
  editorNew,
  showErrorResponse,
  showEvaluate,
  Feedback,
} from '../reducer/editor/feedback'

// evaluate expression (module version)
// returns `Feedback`
export const useEvaluateExpression = (
  projectHash: string,
  code: string
) => {
  const [feedback, setFeedback] =
    React.useState<Feedback>(editorNew)
  const [stale, setStale] = React.useState<boolean>(false)
  const [lastCode, setLastCode] = React.useState('')

  React.useEffect(() => {
    setLastCode(code)
    if (code === '' || code === lastCode) return
    setStale(true)

    evaluateModule({
      emrProjectHash: projectHash,
      emrCode: code,
    })().then((result) =>
      pipe(
        result,
        E.fold<
          UserErrorResponse,
          EvaluateModuleResponse,
          Feedback
        >(
          (e) => showErrorResponse(e),
          (a) =>
            showEvaluate(a.emrExpressionData, a.emrResult)
        ),
        (feedback) => {
          setFeedback(feedback)
          setStale(false)
        }
      )
    )
  }, [code, lastCode, projectHash, setFeedback])

  return [feedback, stale] as const
}
