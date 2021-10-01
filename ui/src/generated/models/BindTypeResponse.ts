/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { DataType } from './DataType'
import type { ExpressionData } from './ExpressionData'
import type { ProjectData } from './ProjectData'
import type { Typeclass } from './Typeclass'

export type BindTypeResponse = {
  btCodegen?: ExpressionData
  btTypeclasses: Array<Typeclass>
  btDataType: DataType
  btProjectData: ProjectData
  btPrettyType: string
}
