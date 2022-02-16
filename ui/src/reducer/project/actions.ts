import {
  ExprHash,
  ProjectData,
  ExpressionData,
} from '../../types'

export const storeProjectData = (
  data: ProjectData,
  extraHashes: ExprHash[] = []
) => ({
  type: 'StoreProjectData' as const,
  data,
  extraHashes,
})

export const initialise = () => ({
  type: 'Initialise' as const,
})

export const createProject = () => ({
  type: 'CreateProject' as const,
})

export const fetchExpressionSuccess = (
  exprHash: ExprHash,
  storeExpression: ExpressionData
) => ({
  type: 'FetchExpressionSuccess' as const,
  exprHash,
  storeExpression,
})

export type ProjectAction =
  | ReturnType<typeof initialise>
  | ReturnType<typeof storeProjectData>
  | ReturnType<typeof createProject>
  | ReturnType<typeof fetchExpressionSuccess>
