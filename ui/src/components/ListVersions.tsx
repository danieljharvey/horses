import * as React from 'react'
import { BindingVersion, ExprHash } from '../types/'
import { Link } from './View/Link'
import { InlineSpaced } from './View/InlineSpaced'

import { Paragraph } from './View/Paragraph'
import { FlexColumnSpaced } from './View/FlexColumnSpaced'

import { getUsagesOfExprHash } from '../reducer/project/selectors'
import { useStore } from '../hooks/useStore'

type ListVersionsProps = {
  versions: BindingVersion[]
  currentHash: ExprHash
  name: string
  onBindingSelect: (
    bindingName: string,
    exprHash: string
  ) => void
}

export const ListVersions: React.FC<ListVersionsProps> = ({
  versions,
  currentHash,
  onBindingSelect,
  name,
}) => {
  const getUsages = useStore(getUsagesOfExprHash)

  if (versions.length < 1) {
    return null
  }

  const usagesOfExprHash = (exprHash: ExprHash) =>
    getUsages(exprHash).length

  return (
    <FlexColumnSpaced>
      <Paragraph>Versions</Paragraph>
      <InlineSpaced>
        {versions.map(({ bvExprHash, bvNumber }) => {
          const title = `Version ${bvNumber}`
          return (
            <Link
              depType="expression"
              number={usagesOfExprHash(bvExprHash)}
              key={title}
              onClick={() =>
                onBindingSelect(name, bvExprHash)
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
