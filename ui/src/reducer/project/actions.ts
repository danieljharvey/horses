import { ExprHash, ProjectData } from '../../types'
import { ProjectAction } from './types'

export const storeProjectData = (
  data: ProjectData,
  extraHashes: ExprHash[] = []
): ProjectAction => ({
  type: 'StoreProjectData',
  data,
  extraHashes,
})
