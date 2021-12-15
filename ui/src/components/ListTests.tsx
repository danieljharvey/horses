import * as React from 'react'
import { PropertyTestData, UnitTestData } from '../types/'
import { PropertyTest } from './PropertyTest'
import { UnitTest } from './UnitTest'
import { FlexColumnSpaced } from './View/FlexColumnSpaced'
import { InlineSpaced } from './View/InlineSpaced'
import { Paragraph } from './View/Paragraph'

type Props = {
  unitTests: UnitTestData[]
  propertyTests: PropertyTestData[]
}

export const ListTests: React.FC<Props> = ({
  unitTests,
  propertyTests,
}) => {
  const utPassing = unitTests.filter(
    (ut) => ut.utdTestSuccess
  )
  const utFailing = unitTests.filter(
    (ut) => !ut.utdTestSuccess
  )
  const ptPassing = propertyTests.filter(
    (pt) => pt.ptdTestFailures.length === 0
  )
  const ptFailing = propertyTests.filter(
    (pt) => pt.ptdTestFailures.length > 0
  )

  if (unitTests.length === 0) {
    return null
  }

  const passing = utPassing.length + ptPassing.length
  const total = unitTests.length + propertyTests.length

  const message = `Tests - ${passing}/${total} pass`

  return (
    <FlexColumnSpaced>
      <Paragraph>{message}</Paragraph>
      <InlineSpaced>
        <>
          {utFailing.map((unitTest, key) => (
            <UnitTest unitTest={unitTest} key={key} />
          ))}
          {utPassing.map((unitTest, key) => (
            <UnitTest unitTest={unitTest} key={key} />
          ))}
          {ptFailing.map((propertyTest, key) => (
            <PropertyTest
              propertyTest={propertyTest}
              key={key}
            />
          ))}
          {ptPassing.map((propertyTest, key) => (
            <PropertyTest
              propertyTest={propertyTest}
              key={key}
            />
          ))}
        </>
      </InlineSpaced>
    </FlexColumnSpaced>
  )
}
