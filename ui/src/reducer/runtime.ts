import { State, Action, Event } from './types'
import { EventReducerRuntime } from '../hooks/useEventReducer'
import { listBindings } from '../service/project'
import { getModule } from '../service/module'

import { ModuleHash } from '../types/'
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

const orEmpty = <A>() =>
  TE.fold(
    () => T.of([]),
    (a: A[]) => T.of(a)
  )

export const runtime =
  (
    history: H.History
  ): EventReducerRuntime<State, Action, Event> =>
  (_state, event) => {
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
    }
  }
