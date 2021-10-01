import * as React from 'react'
import { LoadingPage } from './LoadingPage'
import { ProjectPage } from './ProjectPage'

import {
  Route,
  Switch,
  BrowserRouter,
  useParams,
} from 'react-router-dom'

const ProjectSwitcher = () => {
  // We can use the `useParams` hook here to access
  // the dynamic pieces of the URL.
  const { projectHash } = useParams<{
    projectHash: string
  }>()
  return <ProjectPage projectHash={projectHash} />
}

const NoMatch = () => <p>Could not find route, sorry pal</p>

export const Router = () => (
  <BrowserRouter>
    <Switch>
      <Route
        path="/project/:projectHash"
        children={<ProjectSwitcher />}
      />
      <Route exact path="/">
        <LoadingPage />
      </Route>
      <Route path="*">
        <NoMatch />
      </Route>
    </Switch>
  </BrowserRouter>
)
