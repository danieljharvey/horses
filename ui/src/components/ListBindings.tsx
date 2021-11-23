import * as React from 'react'
import { ExprHash } from '../types/'
import { Link } from './View/Link'
import { InlineSpaced } from './View/InlineSpaced'
import {
  countActiveVersionsOfBinding,
  getUsagesOfExprHash,
} from '../reducer/project/selectors'
import { State } from '../reducer/types'
import { FlexColumnSpaced } from './View/FlexColumnSpaced'
import { Paragraph } from './View/Paragraph'

type ListBindingsProps = {
  values: Record<string, ExprHash>
  types: Record<string, ExprHash>
  onBindingSelect: (
    bindingName: string,
    exprHash: string
  ) => void
  state: State
}

export const ListBindings: React.FC<ListBindingsProps> = ({
  values,
  types,
  onBindingSelect,
  state,
}) => {
  // try and re-use it this where possible
  const items = { ...values, ...types }

  if (Object.keys(items).length < 1) {
    return null
  }

  const getActiveVersions = (bindingName: string) =>
    countActiveVersionsOfBinding(bindingName, state)

  const bindingInUse = (exprHash: ExprHash) =>
    getUsagesOfExprHash(exprHash, state).length > 0

  return (
    <InlineSpaced>
      {Object.entries(values).map(([name, exprHash]) => (
        <Link
          depType="expression"
          number={getActiveVersions(name)}
          key={name}
          onClick={() => onBindingSelect(name, exprHash)}
          highlight={bindingInUse(exprHash)}
        >
          {name}
        </Link>
      ))}
      {Object.entries(types).map(([name, exprHash]) => (
        <Link
          depType="type"
          key={name}
          number={0}
          onClick={() => onBindingSelect(name, exprHash)}
          highlight={bindingInUse(exprHash)}
        >
          {name}
        </Link>
      ))}
    </InlineSpaced>
  )
}
