import * as React from 'react'
import { useCompiledExpression } from '../../hooks/useCompiledExpression'
import { ExprHash } from '../../types'
import { Button } from '../View/Button'
import { ButtonLink } from '../View/ButtonLink'
import { Paragraph } from '../View/Paragraph'

type Props = {
  exprHash: ExprHash
}

export const Compile: React.FC<Props> = ({ exprHash }) => {
  const [compileState, compile] = useCompiledExpression(
    exprHash
  )

  switch (compileState.type) {
    case 'Empty':
      return (
        <Button
          title="Compile"
          onClick={compile}
        >{`Compile`}</Button>
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
          {`Download output`}
        </ButtonLink>
      )
  }
}
