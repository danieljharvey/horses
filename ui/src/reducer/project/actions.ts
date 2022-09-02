import type {
  ExprHash,
  ModuleHash,
  ProjectData,
  ExpressionData,
  GetModuleResponse,
} from '../../types'

export const storeProjectData = (
  data: ProjectData,
  extraHashes: ExprHash[] = []
) => ({
  type: 'StoreProjectData' as const,
  data,
  extraHashes,
})

const fetchModule = (moduleHash: ModuleHash) => ({
  type: 'FetchModule' as const,
  moduleHash,
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

export const fetchModuleSuccess = (
  fetched: GetModuleResponse
) => ({ type: 'FetchModuleSuccess' as const, fetched })

export type ProjectAction =
  | ReturnType<typeof initialise>
  | ReturnType<typeof storeProjectData>
  | ReturnType<typeof fetchExpressionsSuccess>
  | ReturnType<typeof fetchModuleSuccess>
  | ReturnType<typeof fetchModule>
