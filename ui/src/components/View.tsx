import * as React from 'react'
import * as O from 'fp-ts/Option'
import { ScratchModule } from './Editor/ScratchModule'
import { EditBinding } from './Editor/EditBinding'
import { FilteredBindingList } from './FilteredBindingList'
import { Menu } from './Menu'
import {
  getCurrentScreen,
  getLastScreen,
} from '../reducer/view/selectors'
import { pushScreen } from '../reducer/view/actions'
import { editorForBinding } from '../reducer/editor/helpers'
import {
  Screen,
  editScreen,
  editModuleScreen,
} from '../reducer/view/screen'
import { Screen as ScreenComponent } from './View/Screen'
import { PanelRow } from './View/PanelRow'
import { ExprHash, ProjectHash, ModuleHash } from '../types'
import { useDispatch } from '../hooks/useDispatch'
import { useStoreRec } from '../hooks/useStore'
import { getProjectHash } from '../reducer/project/selectors'
import { LoadModule } from './Editor/LoadModule'

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
    modules,
  } = useStoreRec({
    screen: getCurrentScreen,
    lastScreen: getLastScreen,
    getEditorState: editorForBinding,
    bindings: (s) => s.project.bindings,
    typeBindings: (s) => s.project.typeBindings,
    modules: (s) => s.project.modules,
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
      dispatch(pushScreen(editScreen(bindingName, edit)))
    }
  }

  const onModuleSelect = (moduleHash: ModuleHash) => {
    dispatch(pushScreen(editModuleScreen(moduleHash)))
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
              modules={modules}
              onModuleSelect={onModuleSelect}
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
  projectHash: ProjectHash,
  onBindingSelect: (
    bindingName: string,
    exprHash: ExprHash
  ) => void
): [JSX.Element, boolean] => {
  switch (screen.type) {
    case 'scratch-module':
      return [
        <ScratchModule
          projectHash={projectHash}
          editor={screen.editor}
          onBindingSelect={onBindingSelect}
        />,
        true,
      ]

    case 'edit-module':
      return [
        <LoadModule moduleHash={screen.moduleHash} />,
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
  }
}
