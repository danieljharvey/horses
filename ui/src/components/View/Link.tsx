import * as React from 'react'
import './Link.css'

type Props = {
  onClick: () => void
  depType: 'expression' | 'type'
  version: number
}

export const Link: React.FC<Props> = ({
  onClick,
  children,
  depType,
  version,
}) => (
  <p
    onClick={() => onClick()}
    className={`link${version > 1 ? ' old' : ''} ${
      depType === 'expression' ? 'expression' : 'type'
    }`}
  >
    {children}
    {version > 1 && (
      <span className="badge">{`v${version}`}</span>
    )}
  </p>
)
