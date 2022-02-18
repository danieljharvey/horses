import * as React from 'react'
import { ExprHash } from '../types/'
import { InlineSpaced } from './View/InlineSpaced'

import * as O from 'fp-ts/Option'
import { Button } from './View/Button'
import { Paragraph } from './View/Paragraph'
import { FlexColumnSpaced } from './View/FlexColumnSpaced'
import {
  lookupNameForExprHash,
  getVersionsOfBinding,
} from '../reducer/project/selectors'
import { useStoreRec } from '../hooks/useStore'

type UpgradeProps = {
  currentHash: ExprHash
  name: string
  onUpgradeExpression: (bindingName: string) => void
  values: Record<string, ExprHash>
  types: Record<string, ExprHash>
}

export const Upgrade: React.FC<UpgradeProps> = ({
  currentHash,
  name,
  onUpgradeExpression,
  values,
  types,
}) => {
  const { getVersions, lookupName } = useStoreRec({
    getVersions: getVersionsOfBinding,
    lookupName: lookupNameForExprHash,
  })

  const versionsOfBinding = getVersions(name)

  const allDepHashes: ExprHash[] = [
    ...Object.values(values),
    ...Object.values(types),
  ]

  const bindingIsNewest = (exprHash: ExprHash) =>
    O.isSome(lookupName(exprHash))

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
