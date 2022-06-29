import type {
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

export const fetchExpressionsSuccess = (
  fetched: Record<ExprHash, ExpressionData>
) => ({
  type: 'FetchExpressionsSuccess' as const,
  fetched,
})

export type ProjectAction =
  | ReturnType<typeof initialise>
  | ReturnType<typeof storeProjectData>
  | ReturnType<typeof fetchExpressionsSuccess>
