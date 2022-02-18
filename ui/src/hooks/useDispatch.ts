import * as React from 'react'
import { StoreContext } from '../pages/ProjectPage'

export const useDispatch = () => {
  const { dispatch } = React.useContext(StoreContext)
  return dispatch
}
