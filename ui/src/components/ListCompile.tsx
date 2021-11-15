import * as React from 'react'
import { ExprHash } from '../types'
import { InlineSpaced } from './View/InlineSpaced'

import { Paragraph } from './View/Paragraph'
import { FlexColumnSpaced } from './View/FlexColumnSpaced'

import { RuntimeData } from '../generated'
import { Compile } from './Editor/Compile'

type ListCompileProps = {
  runtimes: RuntimeData[]
  exprHash: ExprHash
}

export const ListCompile: React.FC<ListCompileProps> = ({
  runtimes,
  exprHash,
}) => {
  if (runtimes.length < 1) {
    return null
  }

  return (
    <FlexColumnSpaced>
      <Paragraph>Compile</Paragraph>
      <InlineSpaced>
        {runtimes.map((rt) => (
          <Compile
            exprHash={exprHash}
            runtime={rt.rtdName}
            title={rt.rtdName}
          />
        ))}
      </InlineSpaced>
    </FlexColumnSpaced>
  )
}
