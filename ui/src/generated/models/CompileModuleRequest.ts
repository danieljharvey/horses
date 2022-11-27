/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { Backend } from './Backend'

export type CompileModuleRequest = {
  chBackend: Backend
  chModuleHash: string
}
