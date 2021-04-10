import * as React from 'react'
import './ButtonLink.css'

type Props = {
  href: string
  download?: string
}

export const ButtonLink: React.FC<Props> = ({
  href,
  children,
  download,
}) => (
  <a
    className="button-link"
    href={href}
    download={download}
  >
    {children}
  </a>
)
