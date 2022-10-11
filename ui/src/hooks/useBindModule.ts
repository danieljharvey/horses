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
import {
  ExprHash,
  ProjectHash,
  ModuleData,
  TestData,
} from '../types/'

// this is how we should do the screens from now on

export type BindModule = {
  moduleData: ModuleData
  testData: TestData
}

export type BindModuleState = RemoteData<
  UserErrorResponse,
  BindModule
>

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
    React.useState<BindModuleState>(initial)

  const bindNewModule = async () => {
    setModuleState(pending)
    const result = await bindModule({
      bmProjectHash: projectHash,
      bmModule: code,
      bmModuleName: moduleName,
    })()

    pipe(
      result,
      E.fold<
        UserErrorResponse,
        BindModuleResponse,
        BindModuleState
      >(
        (e) => failure(e),
        (a) => {
          updateProject(a.bmProjectData, [])
          return success({
            moduleData: a.bmModuleData,
            testData: a.bmTestData,
          })
        }
      ),
      setModuleState
    )
  }
  return [bindNewModule, moduleState] as const
}
