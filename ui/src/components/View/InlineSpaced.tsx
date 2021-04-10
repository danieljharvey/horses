import * as React from 'react'
import './InlineSpaced.css'

const SpacedContainer: React.FC<{}> = ({ children }) => (
  <div className="spaced-container">{children}</div>
)

export const InlineSpaced: React.FC<{}> = ({
  children,
}) => (
  <section className="inline-spaced">
    {React.Children.map(children, (child, index) => (
      <SpacedContainer key={index}>{child}</SpacedContainer>
    ))}
  </section>
)
