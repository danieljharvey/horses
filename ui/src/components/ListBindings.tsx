import * as React from 'react'
import { ModuleHash } from '../types/'
import { Link } from './View/Link'
import { InlineSpaced } from './View/InlineSpaced'

type ListBindingsProps = {
  modules: Record<string, ModuleHash>
  onModuleSelect: (moduleHash: ModuleHash) => void
}

export const ListBindings: React.FC<ListBindingsProps> = ({
  modules,
  onModuleSelect,
}) => {
  if (Object.keys(modules).length < 1) {
    return null
  }

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
    </InlineSpaced>
  )
}
