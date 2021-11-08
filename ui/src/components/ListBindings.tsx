import * as React from 'react'
import { ExprHash } from '../types/'
import { Link } from './View/Link'
import { InlineSpaced } from './View/InlineSpaced'

type ListBindingsProps = {
  values: Record<string, ExprHash>
  types: Record<string, ExprHash>
  onBindingSelect: (
    bindingName: string,
    exprHash: string
  ) => void
  onFetchExpressionsForHashes: (hashes: ExprHash[]) => void
}

export const ListBindings: React.FC<ListBindingsProps> = ({
  values,
  types,
  onBindingSelect,
  onFetchExpressionsForHashes,
}) => {
  // try and re-use it this where possible
  const items = { ...values, ...types }

  const hashes = Object.values(items)

  React.useEffect(() => {
    if (hashes.length > 0) {
      onFetchExpressionsForHashes(hashes)
    }
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [])

  if (Object.keys(items).length < 1) {
    return null
  }

  const getVersion = (exprHash: string) =>
    Math.random() > 0.5 ? 1 : 2

  return (
    <InlineSpaced>
      {Object.entries(values).map(([name, exprHash]) => (
        <Link
          depType="expression"
          version={getVersion(exprHash)}
          key={name}
          onClick={() => onBindingSelect(name, exprHash)}
        >
          {name}
        </Link>
      ))}
      {Object.entries(types).map(([name, exprHash]) => (
        <Link
          depType="type"
          key={name}
          version={getVersion(exprHash)}
          onClick={() => onBindingSelect(name, exprHash)}
        >
          {name}
        </Link>
      ))}
    </InlineSpaced>
  )
}
