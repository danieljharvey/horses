import * as React from 'react'
import { pipe } from 'fp-ts/function'
import * as O from 'fp-ts/Option'
import { ModuleHash } from '../../types'
import { useModule } from '../../hooks/useModule'
import { EditModule } from './EditModule'

type Props = {
  moduleHash: ModuleHash
}

export const LoadModule: React.FC<Props> = ({
  moduleHash,
}) => {
  const maybeMod = useModule(moduleHash)

  return pipe(
    maybeMod,
    O.match(
      () => <div>Loading</div>,
      ({ geModuleData }) => (
        <EditModule
          onModuleSelect={() => {}}
          moduleData={geModuleData}
        />
      )
    )
  )
}
