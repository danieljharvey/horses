import * as React from 'react'
import './Link.css'

type Props = {
  onClick: () => void
  depType: 'expression' | 'type' | 'module'
  number: number
  highlight: boolean
}

export const Link: React.FC<Props> = ({
  onClick,
  children,
  depType,
  number,
  highlight,
}) => (
  <p
    onClick={() => onClick()}
    className={`link${
      highlight === false ? ' unused' : ''
    } ${depType}`}
  >
    {children}
    {number > 1 && (
      <span className="badge">{`${number}`}</span>
    )}
  </p>
)
