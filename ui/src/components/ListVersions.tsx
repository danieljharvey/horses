import * as React from 'react'
import {
  BindingVersion,
  ExprHash,
  exprHash,
} from '../types/'
import { Link } from './View/Link'
import { InlineSpaced } from './View/InlineSpaced'

import { Paragraph } from './View/Paragraph'
import { FlexColumnSpaced } from './View/FlexColumnSpaced'

type ListVersionsProps = {
  versions: BindingVersion[]
  currentHash: ExprHash
  name: string
  onBindingSelect: (
    bindingName: string,
    exprHash: ExprHash
  ) => void
}

export const ListVersions: React.FC<ListVersionsProps> = ({
  versions,
  currentHash,
  onBindingSelect,
  name,
}) => {
  if (versions.length < 1) {
    return null
  }

  return (
    <FlexColumnSpaced>
      <Paragraph>Versions</Paragraph>
      <InlineSpaced>
        {versions.map(({ bvExprHash, bvNumber }) => {
          const title = `Version ${bvNumber}`
          return (
            <Link
              depType="expression"
              number={1}
              key={title}
              onClick={() =>
                onBindingSelect(name, exprHash(bvExprHash))
              }
              highlight={bvExprHash === currentHash}
            >
              {title}
            </Link>
          )
        })}
      </InlineSpaced>
    </FlexColumnSpaced>
  )
}
