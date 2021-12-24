import { Paragraph } from './Paragraph'
import './Tray.css'

type TrayProps = {
  open: boolean
  title: string
}
export const Tray: React.FC<TrayProps> = ({
  open,
  title,
  children,
}) => {
  return open ? (
    <section className="tray">
      <Paragraph>{title}</Paragraph>
      <section className="tray-inner">{children}</section>
    </section>
  ) : null
}
