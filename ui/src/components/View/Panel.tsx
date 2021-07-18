import * as React from 'react'
import './Panel.css'

type Props = {
  flexGrow?: 1 | 2
  onClick?: (e: React.MouseEvent<HTMLDivElement>) => void
}

export const Panel: React.FC<Props> = ({
  children,
  onClick,
  flexGrow = 1,
}) => (
  <section
    className={flexGrow === 2 ? 'panel flex-2' : 'panel'}
    onClick={onClick}
  >
    {children}
  </section>
)
