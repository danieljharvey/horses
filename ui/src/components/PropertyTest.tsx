import * as React from 'react'
import { PropertyTestData } from '../types'
import { Paragraph } from './View/Paragraph'

type Props = {
  propertyTest: PropertyTestData
}

export const PropertyTest: React.FC<Props> = ({
  propertyTest,
}) => {
  const emoji =
    propertyTest.ptdTestFailures.length === 0 ? '✅' : '❌'
  return (
    <Paragraph>{`${emoji} "${propertyTest.ptdTestName}"`}</Paragraph>
  )
}
