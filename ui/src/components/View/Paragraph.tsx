import * as React from 'react'
import './Paragraph.css'

type Props = {
  onClick?: () => void
  bold?: boolean
}

export const Paragraph: React.FC<Props> = ({
  children,
  onClick,
  bold,
}) => (
  <p
    className={`paragraph${bold ? ' bold' : ''}`}
    onClick={onClick}
  >
    {children}
  </p>
)
