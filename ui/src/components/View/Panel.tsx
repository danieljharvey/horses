import * as React from 'react'
import './Panel.css'

type Props = {
  flexGrow?: 1 | 2
}

export const Panel: React.FC<Props> = ({
  children,
  flexGrow = 1,
}) => (
  <section
    className={flexGrow === 2 ? 'panel flex-2' : 'panel'}
  >
    {children}
  </section>
)
