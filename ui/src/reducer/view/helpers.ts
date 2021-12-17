import {
  PropertyTestData,
  UnitTestData,
} from '../../generated'
import { Screen } from './types'

const getTestName = (
  test: UnitTestData | PropertyTestData
): string =>
  'utdTestName' in test
    ? test.utdTestName
    : test.ptdTestName

export const getScreenTitle = (screen: Screen): string => {
  switch (screen.type) {
    case 'new-expression':
      return 'new'
    case 'new-test':
      return screen.editor.expression.type === 'ShowTest'
        ? 'test: ' +
            getTestName(screen.editor.expression.test)
        : 'new test'
    case 'edit':
      return screen.bindingName
    case 'scratch':
      return 'scratch'
    case 'typeSearch':
      return 'type search'
    case 'new-type':
      return 'new type'
    case 'project-graph':
      return 'project graph'
    case 'expression-graph':
      return screen.bindingName
  }
}
