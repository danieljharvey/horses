import * as React from 'react'
import { ExprHash } from '../types/'
import { InlineSpaced } from './View/InlineSpaced'

import { Paragraph } from './View/Paragraph'
import { FlexColumnSpaced } from './View/FlexColumnSpaced'
import { State } from '../reducer/types'
import { getVersionsOfBinding } from '../reducer/project/selectors'

type UpgradeProps = {
  currentHash: ExprHash
  name: string
  state: State
}

export const Upgrade: React.FC<UpgradeProps> = ({
  currentHash,
  name,
  state,
}) => {
  const versionsOfBinding = getVersionsOfBinding(
    name,
    state
  )

  const matching = versionsOfBinding.find(
    (bindingVersion) =>
      bindingVersion.bvExprHash === currentHash
  )

  if (
    !matching ||
    matching.bvNumber !== versionsOfBinding.length
  ) {
    return null
  }

  return (
    <FlexColumnSpaced>
      <Paragraph>Upgrade</Paragraph>
      <InlineSpaced>
        <p>{`Upgrade ${name}`}</p>
      </InlineSpaced>
    </FlexColumnSpaced>
  )
}
