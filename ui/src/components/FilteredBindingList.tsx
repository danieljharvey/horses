import * as React from 'react'
import { ListBindings } from './ListBindings'
import './FilteredBindingList.css'
import { ModuleHash, ExprHash } from '../types/'
import { TextInput } from './View/TextInput'
import { Panel } from './View/Panel'
import { FlexColumnSpaced } from './View/FlexColumnSpaced'

type Props = {
  values: Record<string, ExprHash>
  types: Record<string, ExprHash>
  modules: Record<string, ModuleHash>
  onBindingSelect: (
    bindingName: string,
    exprHash: ExprHash
  ) => void
  onModuleSelect: (moduleHash: ModuleHash) => void
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

export const FilteredBindingList: React.FC<Props> = ({
  values,
  types,
  modules,
  onBindingSelect,
  onModuleSelect,
}) => {
  const [filterText, setFilterText] = React.useState('')
  const filteredValues = filterRecord(filterText, values)
  const filteredTypes = filterRecord(filterText, types)
  const filteredModules = filterRecord(filterText, modules)

  return (
    <Panel>
      <FlexColumnSpaced>
        <TextInput
          placeholder="Filter binding names"
          value={filterText}
          onChange={setFilterText}
        />
        <ListBindings
          onModuleSelect={onModuleSelect}
          modules={filteredModules}
          onBindingSelect={onBindingSelect}
          values={filteredValues}
          types={filteredTypes}
        />
      </FlexColumnSpaced>
    </Panel>
  )
}
