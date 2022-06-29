export * from '../generated'

type Nominal<T> = {
  readonly symbol: T
}

export type Tagged<Tag extends string, A> = A & Nominal<Tag>

// a store expression
export type ExprHash = Tagged<'ExprHash', string>

export const exprHash = (str: string) => str as ExprHash

// a project hash
export type ProjectHash = Tagged<'ProjectHash', string>

export const projectHash = (str: string) =>
  str as ProjectHash

// a module hash
export type ModuleHash = Tagged<'ModuleHash', string>

export const moduleHash = (str: string) => str as ModuleHash
