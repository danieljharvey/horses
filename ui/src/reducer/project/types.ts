import {
  ExprHash,
  ProjectData,
  ExpressionData,
} from '../../types'

export type ProjectState = {
  store: Record<ExprHash, ExpressionData>
  projectHash: ExprHash
  bindings: Record<string, ExprHash>
  typeBindings: Record<string, ExprHash>
}

export type ProjectAction =
  | { type: 'Initialise' }
  | { type: 'StoreProjectHash'; projectHash: ExprHash }
  | {
      type: 'StoreProjectData'
      data: ProjectData
      extraHashes: ExprHash[]
    }
  | {
      type: 'FetchExpressionsForHashes'
      hashes: ExprHash[]
    }
  | { type: 'CreateProject' }
  | {
      type: 'FetchExpressionSuccess'
      exprHash: ExprHash
      storeExpression: ExpressionData
    }

export type ProjectEvent =
  | { type: 'SaveToSessionStorage'; projectHash: ExprHash }
  | { type: 'CreateProject' }
  | { type: 'ListBindings'; projectHash: ExprHash }
  | {
      type: 'FetchExpressions'
      projectHash: ExprHash
      hashes: ExprHash[]
    }
