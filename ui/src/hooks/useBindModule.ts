import * as React from 'react'
import { bindModule } from '../service/project'
import {
  initial,
  pending,
  RemoteData,
  failure,
  success,
} from '@devexperts/remote-data-ts'
import {
  BindModuleResponse,
  ProjectData,
  UserErrorResponse,
} from '../generated'
import { pipe } from 'fp-ts/function'
import * as E from 'fp-ts/Either'
import { ExprHash, ProjectHash } from '../types/'

// this is how we should do the screens from now on

type BindModule = {
  modulePretty: string
}

type State = RemoteData<UserErrorResponse, BindModule>

export const useBindModule = (
  projectHash: ProjectHash,
  code: string,
  moduleName: string | undefined,
  updateProject: (
    pd: ProjectData,
    hashes: ExprHash[]
  ) => void
) => {
  const [moduleState, setModuleState] =
    React.useState<State>(initial)

  const bindNewModule = async () => {
    setModuleState(pending)
    const result = await bindModule({
      bmProjectHash: projectHash,
      bmModule: code,
      bmModuleName: moduleName,
    })()

    pipe(
      result,
      E.fold<UserErrorResponse, BindModuleResponse, State>(
        (e) => failure(e),
        (a) => {
          updateProject(a.bmProjectData, [])
          return success({
            modulePretty: a.bmModuleData.mdModulePretty,
          })
        }
      ),
      setModuleState
    )
  }
  return [bindNewModule, moduleState] as const
}
