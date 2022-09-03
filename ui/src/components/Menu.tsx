import * as React from 'react'
import './Menu.css'
import { Option, isSome } from 'fp-ts/lib/Option'
import {
  newExpressionScreen,
  newTypeScreen,
  Screen,
  typeSearchScreen,
} from '../reducer/view/screen'
import { getScreenTitle } from '../reducer/view/helpers'
import { MenuBar } from './View/MenuBar'
import { useDispatch } from '../hooks/useDispatch'
import {
  popScreen,
  pushScreen,
} from '../reducer/view/actions'
import { newEditorFromScreen } from '../reducer/editor/helpers'

type Props = {
  screen: Screen
  lastScreen: Option<Screen>
}

export const Menu: React.FC<Props> = ({
  screen,
  children,

  lastScreen,
}) => {
  const dispatch = useDispatch()

  const onPopScreen = () => dispatch(popScreen())

  const onNewBinding = () =>
    dispatch(
      pushScreen(
        newExpressionScreen(newEditorFromScreen(screen))
      )
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
            onClick={onTypeSearch}
          >
            🔎
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
