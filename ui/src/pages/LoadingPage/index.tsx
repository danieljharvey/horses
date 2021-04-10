import * as React from 'react'
import './LoadingPage.css'
import { findProject } from './logic'
import * as E from 'fp-ts/Either'
import { Redirect } from 'react-router'
import { ExprHash } from '../../types/'
import { Button } from '../../components/View/Button'
const RotatingHorse = () => (
  <div className="rotating-horse">🐴</div>
)

// loading page
// * checks URL
// * checks session storage
// * creates new project

type State =
  | { type: 'Loading' }
  | { type: 'LoadingError'; message: string }
  | { type: 'Found'; projectHash: ExprHash }

export const LoadingPage = () => {
  const [state, setState] = React.useState<State>({
    type: 'Loading',
  })

  const findProjectHash = () => {
    setState({ type: 'Loading' })
    findProject().then((either) => {
      if (E.isLeft(either)) {
        setState({
          type: 'LoadingError',
          message: either.left,
        })
      } else {
        setState({
          type: 'Found',
          projectHash: either.right,
        })
      }
    })
  }

  React.useEffect(findProjectHash, [])

  if (state.type === 'Found') {
    return <Redirect to={`/project/${state.projectHash}`} />
  }

  return (
    <section className="loading">
      {state.type === 'LoadingError' ? (
        <div className="loading-error-message">
          <p>{state.message}</p>
          <Button onClick={findProjectHash}>Retry</Button>
        </div>
      ) : (
        <div className="loading-message">
          <RotatingHorse />
          <p>Loading...</p>
          <RotatingHorse />
        </div>
      )}
    </section>
  )
}
