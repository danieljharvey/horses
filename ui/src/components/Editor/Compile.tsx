import * as React from 'react'
import { useCompiledExpression } from '../../hooks/useCompiledExpression'
import { ExprHash } from '../../types'
import { Button } from '../View/Button'
import { ButtonLink } from '../View/ButtonLink'
import { Paragraph } from '../View/Paragraph'

type Props = {
  projectHash: ExprHash
  code: string
  runtimeName: string
  description: string
}

export const Compile: React.FC<Props> = ({
  projectHash,
  code,
  runtimeName,
  description,
}) => {
  const [compileState, compile] = useCompiledExpression(
    projectHash,
    code,
    runtimeName
  )

  if (code.length < 1) {
    return null
  }

  switch (compileState.type) {
    case 'Empty':
      return (
        <Button
          title={description}
          onClick={compile}
        >{`Compile '${runtimeName}' runtime`}</Button>
      )
    case 'Fetching':
    case 'CreatingBlob':
      return (
        <Button onClick={() => {}}>Compiling...</Button>
      )
    case 'Failed':
      return (
        <>
          <Paragraph>Compilation failed!</Paragraph>
          <Button onClick={compile}>Retry</Button>
        </>
      )
    case 'HasBlob':
      return (
        <ButtonLink
          href={compileState.url}
          download="mimsa.zip"
        >
          {`Download '${runtimeName}' output`}
        </ButtonLink>
      )
  }
}
