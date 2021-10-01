/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { RuntimeData } from './RuntimeData'
import type { SourceItem } from './SourceItem'
import type { UnitTestData } from './UnitTestData'

export type ExpressionData = {
  edInput: string
  edType: string
  edRuntimes: Record<string, RuntimeData>
  edBindings: Record<string, string>
  edSourceItems: Array<SourceItem>
  edPretty: string
  edHash: string
  edTypeBindings: Record<string, string>
  edGraphviz: string
  edUnitTests: Array<UnitTestData>
}
