import * as React from 'react'
import * as O from 'fp-ts/Option'
import { Scratch } from './Editor/Scratch'
import { ScratchModule } from './Editor/ScratchModule'
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
import { useDispatch } from '../hooks/useDispatch'
import { useStoreRec } from '../hooks/useStore'
import { getProjectHash } from '../reducer/project/selectors'

type Props = {}

export const View: React.FC<Props> = () => {
  const dispatch = useDispatch()

  const {
    screen,
    lastScreen,
    getEditorState,
    bindings,
    typeBindings,
    projectHash,
  } = useStoreRec({
    screen: getCurrentScreen,
    lastScreen: getLastScreen,
    getEditorState: editorForBinding,
    bindings: (s) => s.project.bindings,
    typeBindings: (s) => s.project.typeBindings,
    projectHash: getProjectHash,
  })

  const onBindingSelect = (
    bindingName: string,
    exprHash: ExprHash
  ) => {
    const edit = O.toNullable(
      getEditorState(bindingName, exprHash)
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
    projectHash,
    onBindingSelect
  )

  return (
    <Menu screen={screen} lastScreen={lastScreen}>
      <ScreenComponent>
        <PanelRow>{inner}</PanelRow>
        {showBindingList && (
          <PanelRow>
            <FilteredBindingList
              values={bindings}
              types={typeBindings}
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
  projectHash: ExprHash,
  onBindingSelect: (
    bindingName: string,
    exprHash: ExprHash
  ) => void
) => {
  switch (screen.type) {
    case 'scratch':
      return [
        <Scratch
          projectHash={projectHash}
          editor={screen.editor}
          onBindingSelect={onBindingSelect}
        />,
        true,
      ]

    case 'scratch-module':
      return [
        <ScratchModule
          projectHash={projectHash}
          editor={screen.editor}
          onBindingSelect={onBindingSelect}
        />,
        true,
      ]

    case 'edit':
      return [
        <EditBinding
          editor={screen.editor}
          onBindingSelect={onBindingSelect}
        />,
        true,
      ]

    case 'new-expression':
      return [
        <NewBinding
          editor={screen.editor}
          onBindingSelect={onBindingSelect}
        />,
        true,
      ]

    case 'typeSearch':
      return [
        <TypeSearch onBindingSelect={onBindingSelect} />,
        false,
      ]

    case 'new-test':
      return [
        <NewTest
          editor={screen.editor}
          onBindingSelect={onBindingSelect}
        />,
        false,
      ]

    case 'new-type':
      return [
        <NewType
          editor={screen.editor}
          onBindingSelect={onBindingSelect}
        />,
        true,
      ]

    case 'project-graph':
      return [<ProjectGraph />, false]

    case 'expression-graph':
      return [
        <ExpressionGraph
          exprHash={screen.exprHash}
          bindingName={screen.bindingName}
        />,
        false,
      ]
  }
}
