import * as React from 'react'
import { ListBindings } from './ListBindings'
import './FilteredBindingList.css'
import { ExprHash } from '../types/'
import { TextInput } from './View/TextInput'
import { Panel } from './View/Panel'
import { FlexColumnSpaced } from './View/FlexColumnSpaced'
import { Paragraph } from './View/Paragraph'
import { InlineSpaced } from './View/InlineSpaced'
import {
  useListProjectTests,
  ListProjectTestsState,
} from '../hooks/useListProjectTests'
import { getProjectHash } from '../reducer/project/selectors'
import {
  fold,
  toNullable,
} from '@devexperts/remote-data-ts'
import { pipe } from 'fp-ts/function'
import { ListTests, testCounts } from './ListTests'
import { useStore } from '../hooks/useStore'
type Item = 'bindings' | 'tests'

type Props = {
  values: Record<string, ExprHash>
  types: Record<string, ExprHash>
  onBindingSelect: (
    bindingName: string,
    exprHash: ExprHash
  ) => void
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
  setShowItems: (item: Item) => void,
  loadingTests: ListProjectTestsState
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
      {testTitle(loadingTests)}
    </Paragraph>
  </InlineSpaced>
)

const renderTests = (loadingTests: ListProjectTestsState) =>
  pipe(
    loadingTests,
    fold(
      () => <div />,
      () => <div />,
      (e) => <Paragraph>{e}</Paragraph>,
      (tests) => (
        <ListTests
          propertyTests={tests.propertyTests}
          unitTests={tests.unitTests}
        />
      )
    )
  )

const testTitle = (testLoad: ListProjectTestsState) => {
  const data = toNullable(testLoad)
  if (!data) {
    return 'Tests -/-'
  }
  const { total, passing } = testCounts(
    data.unitTests,
    data.propertyTests
  )
  const failing = total > passing
  return failing ? `Tests ${passing}/${total}` : 'Tests ✅'
}

export const FilteredBindingList: React.FC<Props> = ({
  values,
  types,
  onBindingSelect,
}) => {
  const [filterText, setFilterText] = React.useState('')
  const filteredValues = filterRecord(filterText, values)
  const filteredTypes = filterRecord(filterText, types)
  const [showItems, setShowItems] =
    React.useState<Item>('bindings')

  const projectHash = useStore(getProjectHash)

  const [loadingTests] = useListProjectTests(projectHash)

  const selector = itemSelect(
    showItems,
    setShowItems,
    loadingTests
  )

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
