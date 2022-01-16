/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { ExpressionData } from './ExpressionData'
import type { FromTo } from './FromTo'
import type { ProjectData } from './ProjectData'

export type UpgradeResponse = {
  upUpgradedDeps: Record<string, FromTo>
  upExpressionData: ExpressionData
  upProjectData: ProjectData
}
