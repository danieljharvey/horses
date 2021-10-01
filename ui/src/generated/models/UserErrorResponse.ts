/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { ErrorLocation } from './ErrorLocation'
import type { TypedHoleResponse } from './TypedHoleResponse'

export type UserErrorResponse = {
  ueText: string
  ueTypedHoles: Array<TypedHoleResponse>
  ueErrorLocations: Array<ErrorLocation>
}
