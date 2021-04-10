import * as React from 'react'
import './Code.css'

type CodeType = 'value' | 'type'

type Props = {
  codeType?: CodeType
}

export const Code: React.FC<Props> = ({
  children,
  codeType = 'value',
}) => (
  <pre
    className={codeType === 'type' ? 'type-signature' : ''}
  >
    <code className="code">{children}</code>
  </pre>
)
