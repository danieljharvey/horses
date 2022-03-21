/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { SourceSpan } from './SourceSpan'

export type TypedHoleResponse = {
  thMonoType: string
  thName: string
  thSourceSpan: SourceSpan
  thSuggestions: Array<string>
}
