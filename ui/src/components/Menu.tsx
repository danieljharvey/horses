import * as React from 'react'
import './Menu.css'
import { Option, isSome } from 'fp-ts/lib/Option'
import { Screen } from '../reducer/view/types'
import { getScreenTitle } from '../reducer/view/helpers'
import { MenuBar } from './View/MenuBar'
import { Action } from '../reducer/types'
import {
  popScreen,
  pushScreen,
} from '../reducer/view/reducer'
import { newEditorFromScreen } from '../reducer/editor/helpers'

type Props = {
  screen: Screen
  dispatch: (a: Action) => void
  lastScreen: Option<Screen>
}

export const Menu: React.FC<Props> = ({
  screen,
  children,

  lastScreen,
  dispatch,
}) => {
  const onPopScreen = () => dispatch(popScreen())

  const onProjectGraph = () =>
    dispatch(pushScreen({ type: 'project-graph' }))

  const onNewBinding = () =>
    dispatch(
      pushScreen({
        type: 'new-expression',
        editor: newEditorFromScreen(screen),
      })
    )

  const onNewTest = () =>
    dispatch(
      pushScreen({
        type: 'new-test',
        editor: newEditorFromScreen(screen),
      })
    )

  const onTypeSearch = () =>
    dispatch(
      pushScreen({
        type: 'typeSearch',
      })
    )

  const onNewType = () =>
    dispatch(
      pushScreen({
        type: 'new-type',
        editor: newEditorFromScreen(screen),
      })
    )
  return (
    <>
      <MenuBar>
        <>
          {isSome(lastScreen) && (
            <p
              className="menu-back"
              onClick={onPopScreen}
            >{`<${getScreenTitle(lastScreen.value)}`}</p>
          )}
          <p className="menu-title">
            {getScreenTitle(screen)}
          </p>
        </>

        <>
          <div
            className="menu-button"
            onClick={onProjectGraph}
          >
            📊
          </div>
          <div
            className="menu-button"
            onClick={onTypeSearch}
          >
            🔎
          </div>
          <div className="menu-button" onClick={onNewTest}>
            🧪
          </div>
          <div className="menu-button" onClick={onNewType}>
            🧷
          </div>
          <div
            className="menu-button"
            onClick={onNewBinding}
          >
            ➕
          </div>
        </>
      </MenuBar>
      {children}
    </>
  )
}
