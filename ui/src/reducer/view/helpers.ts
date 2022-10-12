import { Screen } from './screen'

export const getScreenTitle = (screen: Screen): string => {
  switch (screen.type) {
    case 'scratch-module':
      return 'scratch'
    case 'edit-module':
      return screen.moduleHash
  }
}
