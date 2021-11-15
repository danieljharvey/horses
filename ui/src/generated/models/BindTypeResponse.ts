/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { DataType } from './DataType'
import type { ExpressionData } from './ExpressionData'
import type { ProjectData } from './ProjectData'
import type { Typeclass } from './Typeclass'

export type BindTypeResponse = {
  btTypeclasses: Array<Typeclass>
  btProjectData: ProjectData
  btCodegen?: ExpressionData
  btPrettyType: string
  btDataType: DataType
}
