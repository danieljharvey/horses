/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { DataType__ } from './DataType__';
import type { ExpressionData } from './ExpressionData';
import type { ProjectData } from './ProjectData';
import type { Typeclass } from './Typeclass';

export type BindTypeResponse = {
    btCodegen?: ExpressionData;
    btDataType: DataType__;
    btTypeclasses: Array<Typeclass>;
    btProjectData: ProjectData;
    btPrettyType: string;
}
