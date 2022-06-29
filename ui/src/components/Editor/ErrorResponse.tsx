import * as React from 'react'

import { Paragraph } from '../View/Paragraph'
import { FlexColumnSpaced } from '../View/FlexColumnSpaced'
import type { UserErrorResponse } from '../../types'

type Props = {
  errorResponse: UserErrorResponse
}

export const ErrorResponse: React.FC<Props> = ({
  errorResponse,
}) => {
  return (
    <FlexColumnSpaced>
      <Paragraph>{errorResponse.ueText}</Paragraph>
      {errorResponse.ueTypedHoles.map((th) => (
        <FlexColumnSpaced>
          <Paragraph>{`${th.thName}: ${th.thMonoType}`}</Paragraph>
          {th.thSuggestions.length > 0 && (
            <Paragraph>
              {`  Suggestions: ${th.thSuggestions.join(
                ', '
              )}`}
            </Paragraph>
          )}
        </FlexColumnSpaced>
      ))}
    </FlexColumnSpaced>
  )
}
