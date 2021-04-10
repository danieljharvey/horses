import * as React from 'react'
import './Screen.css'

export const Screen: React.FC<{}> = ({ children }) => (
  <section className="screen">{children}</section>
)
