import { pipe } from 'fp-ts/function'
import { createProject } from '../../service/project'
import { ProjectHash, projectHash } from '../../types/'
import * as E from 'fp-ts/Either'
import { projectGet } from '../../reducer/project/helpers'
import * as TE from 'fp-ts/TaskEither'

const createNewProject: TE.TaskEither<string, ProjectHash> =
  pipe(
    createProject(),
    TE.map((res) => projectHash(res.cpProjectData.pdHash))
  )

const findInSessionStorage: TE.TaskEither<
  string,
  ProjectHash
> = pipe(
  TE.fromEither(
    E.fromOption(
      () => 'Could not find project hash in session storage'
    )(projectGet())
  ),
  TE.map((prj) => prj.hash)
)

// if we have no project, we either:
// a) check in session storage for it
// b) make a new one
// c) if either fails, we return an error that lets us show a retry button

export const findProject = pipe(
  findInSessionStorage,
  TE.alt(() => createNewProject)
)
