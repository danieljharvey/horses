import * as React from 'react'
import { UnitTestData } from '../types'

type Props = {
    unitTest: UnitTestData
}

export const UnitTest: React.FC<Props> = ({ unitTest }) => {
    const emoji = unitTest.utdTestSuccess ? '✅' : '❌'
    return <p>{`${emoji} "${unitTest.utdTestName}"`}</p>
}
