import * as React from 'react'
import { ExprHash } from '../types/'
import { InlineSpaced } from './View/InlineSpaced'

import * as O from 'fp-ts/Option'
import { Button } from './View/Button'
import { Paragraph } from './View/Paragraph'
import { FlexColumnSpaced } from './View/FlexColumnSpaced'
import { lookupNameForExprHash } from '../reducer/project/selectors'
import { useStore } from '../hooks/useStore'

type OptimiseProps = {
  name: string
  onOptimiseExpression: (bindingName: string) => void
  canOptimise: boolean
}

export const Optimise: React.FC<OptimiseProps> = ({
  name,
  onOptimiseExpression,
  canOptimise,
}) => {
  const lookupName = useStore(lookupNameForExprHash)

  const bindingIsNewest = (exprHash: ExprHash) =>
    O.isSome(lookupName(exprHash))

  if (!bindingIsNewest || !canOptimise) {
    return null
  }

  return (
    <FlexColumnSpaced>
      <Paragraph>Optimise</Paragraph>
      <InlineSpaced>
        <Button onClick={() => onOptimiseExpression(name)}>
          {`Optimise ${name}`}
        </Button>
      </InlineSpaced>
    </FlexColumnSpaced>
  )
}
