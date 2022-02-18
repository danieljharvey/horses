import * as React from 'react'
import { ExprHash } from '../types/'
import { Link } from './View/Link'
import { InlineSpaced } from './View/InlineSpaced'
import {
  countActiveVersionsOfBinding,
  lookupNameForExprHash,
} from '../reducer/project/selectors'
import * as O from 'fp-ts/Option'
import { useStoreTuple } from '../hooks/useStore'

type ListBindingsProps = {
  values: Record<string, ExprHash>
  types: Record<string, ExprHash>
  onBindingSelect: (
    bindingName: string,
    exprHash: string
  ) => void
}

export const ListBindings: React.FC<ListBindingsProps> = ({
  values,
  types,
  onBindingSelect,
}) => {
  // try and re-use it this where possible
  const items = { ...values, ...types }

  const [getActiveVersions, lookupName] = useStoreTuple([
    countActiveVersionsOfBinding,
    lookupNameForExprHash,
  ] as const)

  if (Object.keys(items).length < 1) {
    return null
  }

  const bindingIsNewest = (exprHash: ExprHash) =>
    O.isSome(lookupName(exprHash))

  return (
    <InlineSpaced>
      {Object.entries(values).map(([name, exprHash]) => (
        <Link
          depType="expression"
          number={getActiveVersions(name)}
          key={name}
          onClick={() => onBindingSelect(name, exprHash)}
          highlight={bindingIsNewest(exprHash)}
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
          highlight={bindingIsNewest(exprHash)}
        >
          {name}
        </Link>
      ))}
    </InlineSpaced>
  )
}
