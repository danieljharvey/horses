/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { RuntimeData } from './RuntimeData';
import type { SourceItem } from './SourceItem';

export type ExpressionData = {
    edHash: string;
    edRuntimes: Record<string, RuntimeData>;
    edBindings: Record<string, string>;
    edInput: string;
    edPretty: string;
    edType: string;
    edTypeBindings: Record<string, string>;
    edSourceItems: Array<SourceItem>;
    edGraphviz: string;
}
