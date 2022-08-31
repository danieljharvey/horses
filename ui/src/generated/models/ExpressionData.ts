/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { SourceItem } from './SourceItem'

export type ExpressionData = {
  edBindings: Record<string, string>
  edCanOptimise: boolean
  edHash: string
  edInput: string
  edPretty: string
  edSourceItems: Array<SourceItem>
  edType: string
  edTypeBindings: Record<string, string>
  edWarnings: Array<string>
}
