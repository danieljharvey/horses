import * as React from 'react'
import './TextInput.css'

type Props = {
  onChange: (s: string) => void
  value: string
  placeholder?: string
}

export const TextInput: React.FC<Props> = ({
  onChange,
  value,
  placeholder,
}) => (
  <input
    type="text"
    className="text-input"
    placeholder={placeholder}
    value={value}
    onChange={(e) => onChange(e.target.value)}
  />
)
