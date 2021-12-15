/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { ExpressionData } from './ExpressionData';
import type { ProjectData } from './ProjectData';
import type { TestData } from './TestData';

export type BindExpressionResponse = {
    beExpressionData: ExpressionData;
    beTestData: TestData;
    beProjectData: ProjectData;
}
