import * as React from 'react'
import type { ExprUsage } from '../types/'
import { Link } from './View/Link'
import { InlineSpaced } from './View/InlineSpaced'

import { Paragraph } from './View/Paragraph'
import { FlexColumnSpaced } from './View/FlexColumnSpaced'

type ListUsagesProps = {
  usages: ExprUsage[]
  onBindingSelect: (
    bindingName: string,
    exprHash: string
  ) => void
}

export const ListUsages: React.FC<ListUsagesProps> = ({
  usages,
  onBindingSelect,
}) => {
  const directUsages = usages.filter(
    (usage) => usage.euIsDirect
  )
  if (directUsages.length < 1) {
    return null
  }

  return (
    <FlexColumnSpaced>
      <Paragraph>Usages</Paragraph>
      <InlineSpaced>
        {directUsages.map(({ euName, euExprHash }) => (
          <Link
            depType="expression"
            number={0}
            key={euExprHash}
            onClick={() =>
              onBindingSelect(euName, euExprHash)
            }
            highlight={false}
          >
            {euName}
          </Link>
        ))}
      </InlineSpaced>
    </FlexColumnSpaced>
  )
}
