import * as React from 'react'

import { Code } from '../View/Code'
import { Paragraph } from '../View/Paragraph'
import { FlexColumnSpaced } from '../View/FlexColumnSpaced'
import type { ExpressionData } from '../../types'

type Props = {
  expression: ExpressionData
}

// show type and warnings of an expression
export const Expression: React.FC<Props> = ({
  expression,
}) => {
  return (
    <FlexColumnSpaced>
      <Code codeType="type">{expression.edType}</Code>
      {expression.edWarnings.length > 0 && (
        <FlexColumnSpaced>
          {expression.edWarnings.map((warn) => (
            <Paragraph>{warn}</Paragraph>
          ))}
        </FlexColumnSpaced>
      )}
    </FlexColumnSpaced>
  )
}
