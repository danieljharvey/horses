import * as React from 'react'
import { useCompiledExpression } from '../../hooks/useCompiledExpression'
import { ExprHash, Backend } from '../../types'
import { Button } from '../View/Button'
import { ButtonLink } from '../View/ButtonLink'
import { Paragraph } from '../View/Paragraph'

type Props = {
  exprHash: ExprHash
  backend: Backend
  title: string
}

export const Compile: React.FC<Props> = ({
  exprHash,
  backend,
  title,
}) => {
  const [compileState, compile] = useCompiledExpression(
    exprHash,
    backend
  )

  switch (compileState.type) {
    case 'Empty':
      return (
        <Button
          title="Compile"
          onClick={compile}
        >{`Compile ${title}`}</Button>
      )
    case 'Fetching':
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
          {`Download ${title} output`}
        </ButtonLink>
      )
  }
}
