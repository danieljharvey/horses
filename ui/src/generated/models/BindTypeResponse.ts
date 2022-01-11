/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { ExpressionData } from './ExpressionData'
import type { ProjectData } from './ProjectData'
import type { Typeclass } from './Typeclass'

export type BindTypeResponse = {
  btCodegen?: ExpressionData
  btTypeclasses: Array<Typeclass>
  btProjectData: ProjectData
  btPrettyType: string
}
