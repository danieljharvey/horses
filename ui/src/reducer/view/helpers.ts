import { Screen } from './screen'

export const getScreenTitle = (screen: Screen): string => {
  switch (screen.type) {
    case 'new-expression':
      return 'new'
    case 'edit':
      return screen.bindingName
    case 'scratch':
      return 'scratch'
    case 'scratch-module':
      return 'scratch v2'
    case 'edit-module':
      return screen.moduleHash
    case 'typeSearch':
      return 'type search'
    case 'new-type':
      return 'new type'
  }
}
