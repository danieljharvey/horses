import * as React from 'react'
import { PropertyTestData, UnitTestData } from '../types/'
import { PropertyTest } from './PropertyTest'
import { UnitTest } from './UnitTest'
import { FlexColumnSpaced } from './View/FlexColumnSpaced'
import { InlineSpaced } from './View/InlineSpaced'

const testCounts = (
  unitTests: UnitTestData[],
  propertyTests: PropertyTestData[]
) => {
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

  const passing = utPassing.length + ptPassing.length
  const total = unitTests.length + propertyTests.length

  return {
    utPassing,
    utFailing,
    ptPassing,
    ptFailing,
    passing,
    total,
  }
}
type Props = {
  unitTests: UnitTestData[]
  propertyTests: PropertyTestData[]
}

export const ListTests: React.FC<Props> = ({
  unitTests,
  propertyTests,
}) => {
  const {
    total,
    utFailing,
    utPassing,
    ptFailing,
    ptPassing,
  } = testCounts(unitTests, propertyTests)

  return total > 0 ? (
    <FlexColumnSpaced>
      <InlineSpaced>
        <>
          {utFailing.map((unitTest, key) => (
            <UnitTest unitTest={unitTest} key={key} />
          ))}
          {ptFailing.map((propertyTest, key) => (
            <PropertyTest
              propertyTest={propertyTest}
              key={key}
            />
          ))}
          {utPassing.map((unitTest, key) => (
            <UnitTest unitTest={unitTest} key={key} />
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
  ) : null
}
