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

  React.useEffect(() => {
    getModule(moduleHash)().then((result) =>
      pipe(
        result,
        E.fold<
          UserErrorResponse,
          GetModuleResponse,
          O.Option<GetModuleResponse>
        >(() => O.none, O.some),
        setResult
      )
    )
  }, [moduleHash])

  return [result] as const
}
