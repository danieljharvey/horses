import * as React from 'react'
import { UnitTestData } from '../types'
import { Paragraph } from './View/Paragraph'

type Props = {
  unitTest: Pick<
    UnitTestData,
    'utdTestSuccess' | 'utdTestName'
  >
}

export const UnitTest: React.FC<Props> = ({ unitTest }) => {
  const emoji = unitTest.utdTestSuccess ? '✅' : '❌'
  return (
    <Paragraph>{`${emoji} "${unitTest.utdTestName}"`}</Paragraph>
  )
}
