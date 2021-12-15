/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { ErrorLocation } from './ErrorLocation';
import type { TypedHoleResponse } from './TypedHoleResponse';

export type UserErrorResponse = {
    ueErrorLocations: Array<ErrorLocation>;
    ueText: string;
    ueTypedHoles: Array<TypedHoleResponse>;
}
