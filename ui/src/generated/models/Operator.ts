/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

export type Operator =
  | {
      tag: 'Equals'
    }
  | {
      tag: 'Add'
    }
  | {
      tag: 'Subtract'
    }
  | {
      tag: 'StringConcat'
    }
  | {
      tag: 'ArrayConcat'
    }
  | {
      contents: string
      tag: 'Custom'
    }
