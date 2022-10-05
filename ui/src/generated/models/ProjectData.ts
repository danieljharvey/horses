/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { BindingVersion } from './BindingVersion'

export type ProjectData = {
  pdBindings: Record<string, string>
  pdHash: string
  pdModuleBindings: Record<string, string>
  pdTypeBindings: Record<string, string>
  pdVersions: Record<string, Array<BindingVersion>>
}
