import * as React from 'react'
import { ModuleHash, ExprHash } from '../types/'
import { Link } from './View/Link'
import { InlineSpaced } from './View/InlineSpaced'
import { lookupNameForExprHash } from '../reducer/project/selectors'
import * as O from 'fp-ts/Option'
import { useStoreTuple } from '../hooks/useStore'

type ListBindingsProps = {
  values: Record<string, ExprHash>
  types: Record<string, ExprHash>
  modules: Record<string, ModuleHash>
  onBindingSelect: (
    bindingName: string,
    exprHash: ExprHash
  ) => void
  onModuleSelect: (moduleHash: ModuleHash) => void
}

export const ListBindings: React.FC<ListBindingsProps> = ({
  values,
  types,
  onBindingSelect,
  modules,
  onModuleSelect,
}) => {
  const [lookupName] = useStoreTuple([
    lookupNameForExprHash,
  ] as const)

  if (Object.keys(modules).length < 1) {
    return null
  }

  const bindingIsNewest = (exprHash: ExprHash) =>
    O.isSome(lookupName(exprHash))

  return (
    <InlineSpaced>
      {Object.entries(modules).map(([name, moduleHash]) => (
        <Link
          depType="module"
          number={0}
          key={name}
          onClick={() => onModuleSelect(moduleHash)}
          highlight={false}
        >
          {name}
        </Link>
      ))}
      {Object.entries(values).map(([name, exprHash]) => (
        <Link
          depType="expression"
          number={1}
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
