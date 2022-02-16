import { Screen } from './screen'

export const setScreen = (screen: Screen) => ({
  type: 'SetScreen' as const,
  screen,
})

export const pushScreen = (screen: Screen) => ({
  type: 'PushScreen' as const,
  screen,
})

export const popScreen = () => ({
  type: 'PopScreen' as const,
})

export type ViewAction =
  | ReturnType<typeof setScreen>
  | ReturnType<typeof pushScreen>
  | ReturnType<typeof popScreen>
