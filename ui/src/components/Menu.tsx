import * as React from 'react'
import './Menu.css'
import { Option, isSome } from 'fp-ts/lib/Option'
import {
  newExpressionScreen,
  newTestScreen,
  newTypeScreen,
  projectGraphScreen,
  Screen,
  typeSearchScreen,
} from '../reducer/view/screen'
import { getScreenTitle } from '../reducer/view/helpers'
import { MenuBar } from './View/MenuBar'
import { Action } from '../reducer/types'
import {
  popScreen,
  pushScreen,
} from '../reducer/view/actions'
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
    dispatch(pushScreen(projectGraphScreen()))

  const onNewBinding = () =>
    dispatch(
      pushScreen(
        newExpressionScreen(newEditorFromScreen(screen))
      )
    )

  const onNewTest = () =>
    dispatch(
      pushScreen(newTestScreen(newEditorFromScreen(screen)))
    )

  const onTypeSearch = () =>
    dispatch(pushScreen(typeSearchScreen()))

  const onNewType = () =>
    dispatch(
      pushScreen(newTypeScreen(newEditorFromScreen(screen)))
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
