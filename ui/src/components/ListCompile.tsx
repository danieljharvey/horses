import * as React from 'react'
import { ModuleHash } from '../types'
import { InlineSpaced } from './View/InlineSpaced'

import { Paragraph } from './View/Paragraph'
import { FlexColumnSpaced } from './View/FlexColumnSpaced'

import { Backend } from '../generated'
import { Compile } from './Editor/Compile'

type ListCompileProps = {
  moduleHash: ModuleHash
}

export const ListCompile: React.FC<ListCompileProps> = ({
  moduleHash,
}) => {
  const backends: { title: string; be: Backend }[] = [
    { title: 'Typescript', be: 'Typescript' },
    { title: 'Javascript', be: 'ESModulesJS' },
  ]

  return (
    <FlexColumnSpaced>
      <Paragraph>Compile</Paragraph>
      <InlineSpaced>
        {backends.map((be) => (
          <Compile
            moduleHash={moduleHash}
            backend={be.be}
            title={be.title}
          />
        ))}
      </InlineSpaced>
    </FlexColumnSpaced>
  )
}
