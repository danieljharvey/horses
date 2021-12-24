import * as React from 'react'
import { PropertyTestData } from '../types'
import { Code } from './View/Code'
import { FlexColumnSpaced } from './View/FlexColumnSpaced'
import { Paragraph } from './View/Paragraph'
import { Tray } from './View/Tray'

type Props = {
  propertyTest: PropertyTestData
}

export const PropertyTest: React.FC<Props> = ({
  propertyTest: { ptdTestFailures, ptdTestName },
}) => {
  const [open, setOpen] = React.useState(false)
  const success = ptdTestFailures.length === 0
  const emoji = success ? '✅' : open ? '👇' : '❌'

  return (
    <>
      <Paragraph
        onClick={() => setOpen(!open)}
      >{`${emoji} "${ptdTestName}"`}</Paragraph>
      {!success && (
        <Tray title="❌ Failures" open={open}>
          {ptdTestFailures.map((fail) => (
            <Code>{fail}</Code>
          ))}
        </Tray>
      )}
    </>
  )
}
