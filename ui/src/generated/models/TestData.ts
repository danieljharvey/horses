/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { PropertyTestData } from './PropertyTestData'
import type { UnitTestData } from './UnitTestData'

export type TestData = {
  tdPropertyTests: Array<PropertyTestData>
  tdUnitTests: Array<UnitTestData>
}
