import * as React from 'react'
import './FlexColumnSpaced.css'

const VerticallySpacedContainer: React.FC<{}> = ({
  children,
}) => (
  <div className="vertically-spaced-container">
    {children}
  </div>
)

export const FlexColumnSpaced: React.FC<{}> = ({
  children,
}) => (
  <section className="flex-column-spaced">
    {React.Children.map(children, (child, index) => (
      <VerticallySpacedContainer key={index}>
        {child}
      </VerticallySpacedContainer>
    ))}
  </section>
)
