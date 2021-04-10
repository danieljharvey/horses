import * as React from 'react'
import './Paragraph.css'

export const Paragraph: React.FC<{}> = ({ children }) => (
  <p className="paragraph">{children}</p>
)
