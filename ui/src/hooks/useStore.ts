import * as React from 'react'
import { StoreContext } from '../pages/ProjectPage'
import { State } from '../reducer/types'

type BaseSelector = (s: State) => any

type RecordReturns<
  SelectorsRec extends Record<string, BaseSelector>
> = {
  [K in keyof SelectorsRec]: ReturnType<SelectorsRec[K]>
}

// use information from the shared state
// to stop rendering the world every time the state changes
// only return bits we need
export const useStoreRec = <
  SelectorsRec extends Record<string, BaseSelector>
>(
  fns: SelectorsRec
): RecordReturns<SelectorsRec> => {
  const { state } = React.useContext(StoreContext)
  const data = {} as RecordReturns<SelectorsRec>
  Object.entries(fns).forEach(([k, fn]) => {
    data[k as keyof SelectorsRec] = fn(state)
  })
  return data
}

type TupleReturns<
  Tuple extends readonly [...BaseSelector[]]
> = {
  [K in keyof Tuple]: [Tuple[K]] extends [BaseSelector]
    ? ReturnType<Tuple[K]>
    : Tuple[K]
}

// use information from the shared state
// to stop rendering the world every time the state changes
// only return bits we need
export const useStoreTuple = <
  SelectorsTuple extends readonly [...BaseSelector[]]
>(
  fns: SelectorsTuple
): TupleReturns<SelectorsTuple> => {
  const { state } = React.useContext(StoreContext)
  return fns.map((fn) =>
    fn(state)
  ) as any as TupleReturns<SelectorsTuple>
}
