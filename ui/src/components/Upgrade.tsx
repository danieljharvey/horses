import * as React from 'react'
import { ExprHash } from '../types/'
import { InlineSpaced } from './View/InlineSpaced'

import * as O from 'fp-ts/Option'
import { Button } from './View/Button'
import { Paragraph } from './View/Paragraph'
import { FlexColumnSpaced } from './View/FlexColumnSpaced'
import { State } from '../reducer/types'
import {
  lookupNameForExprHash,
  getVersionsOfBinding,
} from '../reducer/project/selectors'

type UpgradeProps = {
  currentHash: ExprHash
  name: string
  state: State
  onUpgradeExpression: (bindingName: string) => void
  values: Record<string, ExprHash>
  types: Record<string, ExprHash>
}

export const Upgrade: React.FC<UpgradeProps> = ({
  currentHash,
  name,
  state,
  onUpgradeExpression,
  values,
  types,
}) => {
  const versionsOfBinding = getVersionsOfBinding(
    name,
    state
  )

  const allDepHashes: ExprHash[] = [
    ...Object.values(values),
    ...Object.values(types),
  ]

  const bindingIsNewest = (exprHash: ExprHash) =>
    O.isSome(lookupNameForExprHash(exprHash, state))

  const upToDateBindings =
    allDepHashes.map(bindingIsNewest).filter((a) => a)
      .length === allDepHashes.length

  const matching = versionsOfBinding.find(
    (bindingVersion) =>
      bindingVersion.bvExprHash === currentHash
  )

  if (
    upToDateBindings ||
    !matching ||
    matching.bvNumber !== versionsOfBinding.length
  ) {
    return null
  }

  return (
    <FlexColumnSpaced>
      <Paragraph>Upgrade</Paragraph>
      <InlineSpaced>
        <Button onClick={() => onUpgradeExpression(name)}>
          {`Upgrade ${name}`}
        </Button>
      </InlineSpaced>
    </FlexColumnSpaced>
  )
}
