import * as NE from 'fp-ts/NonEmptyArray'
import { Screen } from './screen'

export type ViewState = {
  stack: NE.NonEmptyArray<Screen>
}

export type ViewEvent = never
