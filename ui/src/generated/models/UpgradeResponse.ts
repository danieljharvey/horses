/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { ExpressionData } from './ExpressionData'
import type { FromTo } from './FromTo'
import type { ProjectData } from './ProjectData'
import type { TestData } from './TestData'

export type UpgradeResponse = {
  upUpgradedDeps: Record<string, FromTo>
  upTestData: TestData
  upExpressionData: ExpressionData
  upProjectData: ProjectData
}
