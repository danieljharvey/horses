import { Screen } from './types'

export const getScreenTitle = (screen: Screen): string => {
  switch (screen.type) {
    case 'new-expression':
      return 'new'
    case 'new-test':
      return screen.editor.expression.type ===
        'ShowUnitTest'
        ? 'test: ' +
            screen.editor.expression.unitTest.utdTestName
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
