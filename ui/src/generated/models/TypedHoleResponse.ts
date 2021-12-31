/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { SourceSpan } from './SourceSpan'

export type TypedHoleResponse = {
  thName: string
  thMonoType: string
  thSuggestions: Array<string>
  thSourceSpan: SourceSpan
}
