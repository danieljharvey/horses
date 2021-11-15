/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { SourceSpan } from './SourceSpan';

export type TypedHoleResponse = {
    thName: string;
    thMonoType: string;
    thSourceSpan: SourceSpan;
    thSuggestions: Array<string>;
}
