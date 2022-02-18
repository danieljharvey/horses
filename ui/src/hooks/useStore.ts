import * as React from 'react'
import { StoreContext } from '../pages/ProjectPage'
import { State } from '../reducer/types'

type Returns<
  SelectorsRec extends Record<string, (s: State) => any>
> = {
  [K in keyof SelectorsRec]: ReturnType<SelectorsRec[K]>
}

// use information from the shared state
// to stop rendering the world every time the state changes
// only return bits we need
export const useStoreRec = <
  SelectorsRec extends Record<string, (s: State) => any>
>(
  fns: SelectorsRec
): Returns<SelectorsRec> => {
  const { state } = React.useContext(StoreContext)
  const data = {} as Returns<SelectorsRec>
  Object.entries(fns).forEach(([k, fn]) => {
    data[k as keyof SelectorsRec] = fn(state)
  })
  return data
}

// use information from the shared state
// to stop rendering the world every time the state changes
// only return bits we need
export const useStore = <A>(fn: (s: State) => A): A => {
  const { state } = React.useContext(StoreContext)
  return fn(state)
}
