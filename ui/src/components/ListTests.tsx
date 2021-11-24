import * as React from 'react'
import { UnitTestData } from '../types/'
import { UnitTest } from './UnitTest'
import { FlexColumnSpaced } from './View/FlexColumnSpaced'
import { InlineSpaced } from './View/InlineSpaced'
import { Paragraph } from './View/Paragraph'

type Props = {
  unitTests: Pick<
    UnitTestData,
    'utdTestSuccess' | 'utdTestName'
  >[]
}

export const ListTests: React.FC<Props> = ({
  unitTests,
}) => {
  const passing = unitTests.filter(
    (ut) => ut.utdTestSuccess
  )
  const failing = unitTests.filter(
    (ut) => !ut.utdTestSuccess
  )

  if (unitTests.length === 0) {
    return null
  }

  const message = `Tests - ${passing.length}/${unitTests.length} pass`
  return (
    <FlexColumnSpaced>
      <Paragraph>{message}</Paragraph>
      <InlineSpaced>
        <>
          {failing.map((unitTest, key) => (
            <UnitTest unitTest={unitTest} key={key} />
          ))}
          {passing.map((unitTest, key) => (
            <UnitTest unitTest={unitTest} key={key} />
          ))}
        </>
      </InlineSpaced>
    </FlexColumnSpaced>
  )
}
