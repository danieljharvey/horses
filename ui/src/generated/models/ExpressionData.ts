/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { RuntimeData } from './RuntimeData';
import type { SourceItem } from './SourceItem';
import type { UnitTestData } from './UnitTestData';

export type ExpressionData = {
    edHash: string;
    edUnitTests: Array<UnitTestData>;
    edRuntimes: Record<string, RuntimeData>;
    edBindings: Record<string, string>;
    edInput: string;
    edPretty: string;
    edType: string;
    edTypeBindings: Record<string, string>;
    edSourceItems: Array<SourceItem>;
    edGraphviz: string;
}
