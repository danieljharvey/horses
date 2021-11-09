import * as React from 'react'
import './Link.css'

type Props = {
  onClick: () => void
  depType: 'expression' | 'type'
  versions: number
  inUse: boolean
}

export const Link: React.FC<Props> = ({
  onClick,
  children,
  depType,
  versions,
  inUse,
}) => (
  <p
    onClick={() => onClick()}
    className={`link${inUse === false ? ' unused' : ''} ${
      depType === 'expression' ? 'expression' : 'type'
    }`}
  >
    {children}
    {versions > 1 && (
      <span className="badge">{`${versions}`}</span>
    )}
  </p>
)
