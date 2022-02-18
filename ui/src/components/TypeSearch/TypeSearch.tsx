import * as React from 'react'
import { ListBindings } from '../ListBindings'

import { typeSearch } from '../../service/search'
import * as E from 'fp-ts/Either'
import { TextInput } from '../View/TextInput'
import { Code } from '../View/Code'
import { Paragraph } from '../View/Paragraph'
import { Panel } from '../View/Panel'
import { ExprHash } from '../../types'
import { FlexColumnSpaced } from '../View/FlexColumnSpaced'
import { useStore } from '../../hooks/useStore'
import { getProjectHash } from '../../reducer/project/selectors'

type Props = {
  onBindingSelect: (
    bindingName: string,
    exprHash: ExprHash
  ) => void
}

export const TypeSearch: React.FC<Props> = ({
  onBindingSelect,
}) => {
  const [searchText, setSearchText] = React.useState('')
  const [items, setItems] = React.useState<string[]>([])
  const [errorMessage, setErrorMessage] = React.useState('')

  const projectHash = useStore(getProjectHash)

  React.useEffect(() => {
    typeSearch({
      tsProjectHash: projectHash,
      tsInput: searchText,
    })().then((res) => {
      if (E.isRight(res)) {
        setItems(res.right.tsProjectMatches)
        setErrorMessage('')
      } else {
        setErrorMessage(res.left)
      }
    })
  }, [searchText, projectHash])

  const filteredValues = items.reduce(
    (list, item) => ({ ...list, [item]: '' }),
    {}
  )
  const filteredTypes = {}

  return (
    <>
      <Panel>
        <FlexColumnSpaced>
          <TextInput
            value={searchText}
            onChange={setSearchText}
          />

          <Code>{`Example: Int -> Int`}</Code>

          {errorMessage.length > 0 &&
          searchText.length > 0 ? (
            <Code>{errorMessage}</Code>
          ) : (
            <FlexColumnSpaced>
              <Paragraph>{`${items.length} matches`}</Paragraph>
              <ListBindings
                onBindingSelect={onBindingSelect}
                values={filteredValues}
                types={filteredTypes}
              />
            </FlexColumnSpaced>
          )}
        </FlexColumnSpaced>
      </Panel>
    </>
  )
}
