import * as React from 'react'
import './MenuBar.css'

const MenuSection: React.FC<{}> = ({ children }) => (
  <div className="menu-section">{children}</div>
)

export const MenuBar: React.FC<{}> = ({ children }) => (
  <section className="menu-bar">
    {children &&
      React.Children.map(children, (item, index) => (
        <MenuSection key={index}>{item}</MenuSection>
      ))}
  </section>
)
