import * as React from 'react'
import { State } from '../../reducer/types'
import { ListBindings } from '../ListBindings'

import { typeSearch } from '../../service/search'
import * as E from 'fp-ts/Either'
import { TextInput } from '../View/TextInput'
import { Code } from '../View/Code'
import { Paragraph } from '../View/Paragraph'
import { Panel } from '../View/Panel'
import { ExprHash } from '../../types'
import { FlexColumnSpaced } from '../View/FlexColumnSpaced'

type Props = {
  state: State
  onBindingSelect: (
    bindingName: string,
    exprHash: ExprHash
  ) => void
}

export const TypeSearch: React.FC<Props> = ({
  state,
  onBindingSelect,
}) => {
  const [searchText, setSearchText] = React.useState('')
  const [items, setItems] = React.useState<string[]>([])
  const [errorMessage, setErrorMessage] = React.useState('')

  React.useEffect(() => {
    typeSearch({
      tsProjectHash: state.project.projectHash,
      tsInput: searchText,
    })().then((res) => {
      if (E.isRight(res)) {
        setItems(res.right.tsProjectMatches)
        setErrorMessage('')
      } else {
        setErrorMessage(res.left)
      }
    })
  }, [searchText, state.project.projectHash])

  const filteredValues = items.reduce(
    (list, item) => ({ ...list, [item]: '' }),
    {}
  )
  const filteredTypes = {}

  return (
    <>
      <Panel>
        <TextInput
          value={searchText}
          onChange={setSearchText}
        />
        <FlexColumnSpaced>
          <Code>{`Example: Int -> Int`}</Code>

          {errorMessage.length > 0 &&
          searchText.length > 0 ? (
            <Code>{errorMessage}</Code>
          ) : (
            <FlexColumnSpaced>
              <Paragraph>{`${items.length} matches`}</Paragraph>
              <ListBindings
                state={state}
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
