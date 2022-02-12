import * as React from 'react'
import { ExprHash } from '../types/'
import { InlineSpaced } from './View/InlineSpaced'

import * as O from 'fp-ts/Option'
import { Button } from './View/Button'
import { Paragraph } from './View/Paragraph'
import { FlexColumnSpaced } from './View/FlexColumnSpaced'
import { State } from '../reducer/types'
import { lookupNameForExprHash } from '../reducer/project/selectors'

type OptimiseProps = {
  name: string
  state: State
  onOptimiseExpression: (bindingName: string) => void
  canOptimise: boolean
}

export const Optimise: React.FC<OptimiseProps> = ({
  name,
  state,
  onOptimiseExpression,
  canOptimise,
}) => {
  const bindingIsNewest = (exprHash: ExprHash) =>
    O.isSome(lookupNameForExprHash(exprHash, state))

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
