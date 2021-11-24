import * as React from 'react'
import { ListBindings } from './ListBindings'
import './FilteredBindingList.css'
import { ExprHash, UnitTestData } from '../types/'
import { TextInput } from './View/TextInput'
import { Panel } from './View/Panel'
import { State } from '../reducer/types'
import { FlexColumnSpaced } from './View/FlexColumnSpaced'
import { Paragraph } from './View/Paragraph'
import { InlineSpaced } from './View/InlineSpaced'
import {
  useListTests,
  ListTestsState,
} from '../hooks/useListTests'
import { getProjectHash } from '../reducer/project/selectors'
import { fold, map } from '@devexperts/remote-data-ts'
import { pipe } from 'fp-ts/function'
import { ListTests } from './ListTests'
type Item = 'bindings' | 'tests'

type Props = {
  values: Record<string, ExprHash>
  types: Record<string, ExprHash>
  onBindingSelect: (
    bindingName: string,
    exprHash: ExprHash
  ) => void
  state: State
}

const filterRecord = <A,>(
  filter: string,
  record: Record<string, A>
): Record<string, A> =>
  filter.length < 1
    ? record
    : objectKeyFilter(
        (str) =>
          filter
            .toLowerCase()
            .split('')
            .reduce(
              (keep, char) =>
                keep && str.toLowerCase().includes(char),
              true as boolean
            ),
        record
      )

const objectKeyFilter = <A,>(
  pred: (key: string) => boolean,
  record: Record<string, A>
): Record<string, A> =>
  Object.keys(record).reduce((newRecord, key) => {
    if (pred(key)) {
      return { ...newRecord, [key]: record[key] }
    }
    return newRecord
  }, {})

const itemSelect = (
  showItem: Item,
  setShowItems: (item: Item) => void
) => (
  <InlineSpaced>
    <Paragraph
      bold={showItem === 'bindings'}
      onClick={() => setShowItems('bindings')}
    >
      Bindings
    </Paragraph>
    <Paragraph
      bold={showItem === 'tests'}
      onClick={() => setShowItems('tests')}
    >
      Tests
    </Paragraph>
  </InlineSpaced>
)

const renderTests = (loadingTests: ListTestsState) =>
  pipe(
    loadingTests,
    map(({ unitTests }): UnitTestData[] =>
      unitTests.map((ut) => ({
        utdTestName: ut.utName,
        utdBindings: {},
        utdTestSuccess: ut.utSuccess,
      }))
    ),
    fold(
      () => <div />,
      () => <div />,
      (e) => <Paragraph>{e}</Paragraph>,
      (unitTests) => <ListTests unitTests={unitTests} />
    )
  )

export const FilteredBindingList: React.FC<Props> = ({
  values,
  types,
  onBindingSelect,
  state,
}) => {
  const [filterText, setFilterText] = React.useState('')
  const filteredValues = filterRecord(filterText, values)
  const filteredTypes = filterRecord(filterText, types)
  const [showItems, setShowItems] =
    React.useState<Item>('bindings')

  const selector = itemSelect(showItems, setShowItems)

  const projectHash = getProjectHash(state)

  const [loadingTests] = useListTests(projectHash)

  return (
    <Panel>
      {selector}

      {showItems === 'bindings' ? (
        <FlexColumnSpaced>
          <TextInput
            placeholder="Filter binding names"
            value={filterText}
            onChange={setFilterText}
          />
          <ListBindings
            onBindingSelect={onBindingSelect}
            values={filteredValues}
            types={filteredTypes}
            state={state}
          />
        </FlexColumnSpaced>
      ) : (
        <FlexColumnSpaced>
          {renderTests(loadingTests)}
        </FlexColumnSpaced>
      )}
    </Panel>
  )
}
