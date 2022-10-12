import { ProjectHash, projectHash } from '../../types'
import { State } from '../types'

export const getProjectHash = (state: State): ProjectHash =>
  projectHash(state.project.projectHash)
