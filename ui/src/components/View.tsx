import * as React from 'react'
import { ScratchModule } from './Editor/ScratchModule'
import { FilteredBindingList } from './FilteredBindingList'
import { Menu } from './Menu'
import {
  getCurrentScreen,
  getLastScreen,
} from '../reducer/view/selectors'
import { pushScreen } from '../reducer/view/actions'
import {
  Screen,
  editModuleScreen,
} from '../reducer/view/screen'
import { Screen as ScreenComponent } from './View/Screen'
import { PanelRow } from './View/PanelRow'
import { ProjectHash, ModuleHash } from '../types'
import { useDispatch } from '../hooks/useDispatch'
import { useStoreRec } from '../hooks/useStore'
import { getProjectHash } from '../reducer/project/selectors'
import { LoadModule } from './Editor/LoadModule'

type Props = {}

export const View: React.FC<Props> = () => {
  const dispatch = useDispatch()

  const { screen, lastScreen, projectHash, modules } =
    useStoreRec({
      screen: getCurrentScreen,
      lastScreen: getLastScreen,
      modules: (s) => s.project.modules,
      projectHash: getProjectHash,
    })

  const onModuleSelect = (moduleHash: ModuleHash) => {
    dispatch(pushScreen(editModuleScreen(moduleHash)))
  }

  const [inner, showBindingList] = getScreenInner(
    screen,
    projectHash
  )

  return (
    <Menu screen={screen} lastScreen={lastScreen}>
      <ScreenComponent>
        <PanelRow>{inner}</PanelRow>
        {showBindingList && (
          <PanelRow>
            <FilteredBindingList
              modules={modules}
              onModuleSelect={onModuleSelect}
            />
          </PanelRow>
        )}
      </ScreenComponent>
    </Menu>
  )
}

const getScreenInner = (
  screen: Screen,
  projectHash: ProjectHash
): [JSX.Element, boolean] => {
  switch (screen.type) {
    case 'scratch-module':
      return [
        <ScratchModule
          projectHash={projectHash}
          editor={screen.editor}
        />,
        true,
      ]

    case 'edit-module':
      return [
        <LoadModule moduleHash={screen.moduleHash} />,
        true,
      ]
  }
}
