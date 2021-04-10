import * as React from 'react'
import './Button.css'

type Props = {
  onClick: () => void
  title?: string
}

export const Button: React.FC<Props> = ({
  onClick,
  children,
  title,
}) => (
  <button
    className="button"
    title={title}
    onClick={() => onClick()}
  >
    {children}
  </button>
)
