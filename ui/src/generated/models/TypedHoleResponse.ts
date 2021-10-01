/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { SourceSpan } from './SourceSpan'

export type TypedHoleResponse = {
  thSuggestions: Array<string>
  thSourceSpan: SourceSpan
  thMonoType: string
  thName: string
}
