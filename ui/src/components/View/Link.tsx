import * as React from 'react'
import './Link.css'

type Props = {
  onClick: () => void
}

export const Link: React.FC<Props> = ({
  onClick,
  children,
}) => (
  <p onClick={() => onClick()} className="link">
    {children}
  </p>
)
