/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { ExpressionData } from './ExpressionData'
import type { FromTo } from './FromTo'
import type { ProjectData } from './ProjectData'
import type { TestData } from './TestData'

export type UpgradeResponse = {
  upExpressionData: ExpressionData
  upProjectData: ProjectData
  upTestData: TestData
  upUpgradedDeps: Record<string, FromTo>
}
