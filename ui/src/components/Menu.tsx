import * as React from 'react'
import './Menu.css'
import { Option, isSome } from 'fp-ts/lib/Option'
import { Screen } from '../reducer/view/screen'
import { getScreenTitle } from '../reducer/view/helpers'
import { MenuBar } from './View/MenuBar'
import { useDispatch } from '../hooks/useDispatch'
import { popScreen } from '../reducer/view/actions'

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
      </MenuBar>
      {children}
    </>
  )
}
