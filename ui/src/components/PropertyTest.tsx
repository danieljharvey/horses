import * as React from 'react'
import { PropertyTestData } from '../types'
import { FlexColumnSpaced } from './View/FlexColumnSpaced'
import { Paragraph } from './View/Paragraph'

type Props = {
  propertyTest: PropertyTestData
}

export const PropertyTest: React.FC<Props> = ({
  propertyTest: { ptdTestFailures, ptdTestName },
}) => {
  const success = ptdTestFailures.length === 0
  const emoji = success ? '✅' : '❌'

  return (
    <FlexColumnSpaced>
      <Paragraph>{`${emoji} "${ptdTestName}"`}</Paragraph>
      {!success && <Paragraph> Failures:</Paragraph>}
      {ptdTestFailures.map((fail) => (
        <Paragraph>{`   ${fail}`}</Paragraph>
      ))}
    </FlexColumnSpaced>
  )
}
