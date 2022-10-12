import * as O from 'fp-ts/Option'
import { pipe } from 'fp-ts/function'
import { ProjectHash } from '../../types'

const safeSessionStorageGet = (
  key: string
): O.Option<string> =>
  O.fromNullable(sessionStorage.getItem(key))

const safeDecode = <A>(str: string): O.Option<A> => {
  try {
    const a = JSON.parse(str)
    return O.some(a)
  } catch {
    return O.none
  }
}

const sessionStorageKey = 'project'

type Project = { hash: ProjectHash }

export const projectSet = (project: Project) =>
  sessionStorage.setItem(
    sessionStorageKey,
    JSON.stringify(project)
  )

export const projectGet = (): O.Option<Project> =>
  pipe(
    safeSessionStorageGet(sessionStorageKey),
    O.chain((str) => safeDecode<Project>(str))
  )
