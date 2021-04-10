import * as React from 'react'
import './PanelRow.css'

export const PanelRow: React.FC<{}> = ({ children }) => (
  <section className="panel-row">{children}</section>
)
