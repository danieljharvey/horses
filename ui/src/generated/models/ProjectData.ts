/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { BindingVersion } from './BindingVersion';
import type { ExprUsage } from './ExprUsage';

export type ProjectData = {
    pdBindings: Record<string, string>;
    pdUsages: Record<string, Array<ExprUsage>>;
    pdVersions: Record<string, Array<BindingVersion>>;
    pdHash: string;
    pdTypeBindings: Record<string, string>;
}
