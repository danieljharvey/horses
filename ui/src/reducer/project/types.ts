import {
  ExprHash,
  ProjectData,
  ExpressionData,
  BindingVersion,
  ExprUsage,
  TestData,
} from '../../types'

export type StoreItem = {
  expression: ExpressionData
  tests: TestData
}

export type ProjectState = {
  store: Record<ExprHash, StoreItem>
  projectHash: ExprHash
  bindings: Record<string, ExprHash>
  typeBindings: Record<string, ExprHash>
  versions: Record<string, BindingVersion[]>
  usages: Record<ExprHash, ExprUsage[]>
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
      testData: TestData
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
