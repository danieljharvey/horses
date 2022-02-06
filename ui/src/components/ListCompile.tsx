import * as React from 'react'
import { ExprHash } from '../types'
import { InlineSpaced } from './View/InlineSpaced'

import { Paragraph } from './View/Paragraph'
import { FlexColumnSpaced } from './View/FlexColumnSpaced'

import { Backend } from '../generated'
import { Compile } from './Editor/Compile'

type ListCompileProps = {
  exprHash: ExprHash
}

export const ListCompile: React.FC<ListCompileProps> = ({
  exprHash,
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
            exprHash={exprHash}
            backend={be.be}
            title={be.title}
          />
        ))}
      </InlineSpaced>
    </FlexColumnSpaced>
  )
}
