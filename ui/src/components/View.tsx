import * as React from 'react'
import * as O from 'fp-ts/Option'
import { State, Action } from '../reducer/types'
import { Scratch } from './Editor/Scratch'
import { EditBinding } from './Editor/EditBinding'
import { NewBinding } from './Editor/NewBinding'
import { NewTest } from './Editor/NewTest'
import { NewType } from './Editor/NewType'
import { ProjectGraph } from './ProjectGraph'
import { ExpressionGraph } from './ExpressionGraph'

import { TypeSearch } from './TypeSearch/TypeSearch'
import { FilteredBindingList } from './FilteredBindingList'
import { Menu } from './Menu'
import {
  getCurrentScreen,
  getLastScreen,
} from '../reducer/view/selectors'
import { pushScreen } from '../reducer/view/actions'
import { editorForBinding } from '../reducer/editor/helpers'
import { Screen } from '../reducer/view/screen'
import { Screen as ScreenComponent } from './View/Screen'
import { PanelRow } from './View/PanelRow'
import { ExprHash } from '../types'

type Props = {
  state: State
  dispatch: (a: Action) => void
}

export const View: React.FC<Props> = ({
  state,
  dispatch,
}) => {
  const screen = getCurrentScreen(state)

  const lastScreen = getLastScreen(state)

  const onBindingSelect = (
    bindingName: string,
    exprHash: ExprHash
  ) => {
    const edit = O.toNullable(
      editorForBinding(bindingName, exprHash, state)
    )
    if (edit) {
      dispatch(
        pushScreen({
          type: 'edit',
          bindingName,
          editor: edit,
        })
      )
    }
  }

  const [inner, showBindingList] = getScreenInner(
    screen,
    state,
    dispatch,
    onBindingSelect
  )

  return (
    <Menu
      screen={screen}
      lastScreen={lastScreen}
      dispatch={dispatch}
    >
      <ScreenComponent>
        <PanelRow>{inner}</PanelRow>
        {showBindingList && (
          <PanelRow>
            <FilteredBindingList
              state={state}
              values={state.project.bindings}
              types={state.project.typeBindings}
              onBindingSelect={onBindingSelect}
            />
          </PanelRow>
        )}
      </ScreenComponent>
    </Menu>
  )
}

const getScreenInner = (
  screen: Screen,
  state: State,
  dispatch: (a: Action) => void,
  onBindingSelect: (
    bindingName: string,
    exprHash: ExprHash
  ) => void
) => {
  switch (screen.type) {
    case 'scratch':
      return [
        <Scratch
          state={state}
          projectHash={state.project.projectHash}
          dispatch={dispatch}
          editor={screen.editor}
          onBindingSelect={onBindingSelect}
        />,
        true,
      ]

    case 'edit':
      return [
        <EditBinding
          state={state}
          dispatch={dispatch}
          editor={screen.editor}
          onBindingSelect={onBindingSelect}
        />,
        true,
      ]

    case 'new-expression':
      return [
        <NewBinding
          state={state}
          dispatch={dispatch}
          editor={screen.editor}
          onBindingSelect={onBindingSelect}
        />,
        true,
      ]

    case 'typeSearch':
      return [
        <TypeSearch
          state={state}
          onBindingSelect={onBindingSelect}
        />,
        false,
      ]

    case 'new-test':
      return [
        <NewTest
          state={state}
          dispatch={dispatch}
          editor={screen.editor}
          onBindingSelect={onBindingSelect}
        />,
        false,
      ]

    case 'new-type':
      return [
        <NewType
          state={state}
          dispatch={dispatch}
          editor={screen.editor}
          onBindingSelect={onBindingSelect}
        />,
        true,
      ]

    case 'project-graph':
      return [
        <ProjectGraph dispatch={dispatch} state={state} />,
        false,
      ]

    case 'expression-graph':
      return [
        <ExpressionGraph
          state={state}
          exprHash={screen.exprHash}
          dispatch={dispatch}
          bindingName={screen.bindingName}
        />,
        false,
      ]
  }
}
