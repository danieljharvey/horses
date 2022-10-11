import * as React from 'react'
import * as O from 'fp-ts/Option'
import { getModule } from '../service/module'
import {
  GetModuleResponse,
  UserErrorResponse,
} from '../generated'
import { ModuleHash } from '../types'
import { pipe } from 'fp-ts/function'
import * as E from 'fp-ts/Either'

// fetch module from API and/or store
export const useModule = (moduleHash: ModuleHash) => {
  const [result, setResult] = React.useState<
    O.Option<GetModuleResponse>
  >(O.none)
  console.log({ result, moduleHash })

  React.useEffect(() => {
    getModule(moduleHash)().then((newResult) =>
      pipe(
        newResult,
        E.fold<
          UserErrorResponse,
          GetModuleResponse,
          O.Option<GetModuleResponse>
        >(
          () => O.none,
          (modResponse) => {
            return O.some(modResponse)
          }
        ),
        setResult
      )
    )
  }, [moduleHash, setResult, getModule])

  return result
}
