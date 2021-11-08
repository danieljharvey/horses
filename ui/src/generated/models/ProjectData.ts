/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { BindingVersion } from './BindingVersion'
import type { Usage } from './Usage'

export type ProjectData = {
  pdBindings: Record<string, string>
  pdUsages: Record<string, Array<Usage>>
  pdVersions: Record<string, Array<BindingVersion>>
  pdHash: string
  pdTypeBindings: Record<string, string>
}
