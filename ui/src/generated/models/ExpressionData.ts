/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { RuntimeData } from './RuntimeData';
import type { UnitTestData } from './UnitTestData';

export type ExpressionData = {
    edType: string;
    edRuntimes: Record<string, RuntimeData>;
    edBindings: Record<string, string>;
    edPretty: string;
    edTypeBindings: Record<string, string>;
    edHash: string;
    edGraphviz: string;
    edUnitTests: Array<UnitTestData>;
}
