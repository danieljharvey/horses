/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { ExpressionData } from './ExpressionData';
import type { ProjectData } from './ProjectData';

export type BindExpressionResponse = {
    beUpdatedTestsCount: number;
    beExpressionData: ExpressionData;
    beProjectData: ProjectData;
}
