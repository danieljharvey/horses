/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { SourceItem } from './SourceItem'

export type ExpressionData = {
  edHash: string
  edBindings: Record<string, string>
  edInput: string
  edType: string
  edSourceItems: Array<SourceItem>
  edPretty: string
  edTypeBindings: Record<string, string>
  edWarnings: Array<string>
  edGraphviz: string
}
