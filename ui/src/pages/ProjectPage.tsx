import * as React from 'react'
import { useEventReducer } from '../hooks/useEventReducer'
import { eventReducer, initialState } from '../reducer/root'
import { runtime } from '../reducer/runtime'
import { View } from '../components/View'
import '../App.css'
import { useHistory } from 'react-router-dom'
import { initialise } from '../reducer/project/actions'
import { Action } from '../reducer/types'

interface Props {
  projectHash: string
}

type Context = {
  state: ReturnType<typeof initialState>
  dispatch: (a: Action) => void
}

export const StoreContext = React.createContext<Context>({
  state: initialState(''),
  dispatch: () => {},
})

// this doesn't feel right but YOLO
export const ProjectPage: React.FC<Props> = ({
  projectHash,
}) => {
  const history = useHistory()

  const [state, dispatch] = useEventReducer(
    eventReducer,
    initialState(projectHash),
    runtime(history)
  )

  React.useEffect(() => {
    dispatch(initialise())
    /* eslint-disable-next-line react-hooks/exhaustive-deps */
  }, [])

  return (
    <StoreContext.Provider value={{ state, dispatch }}>
      <div className="App">
        <View state={state} />
      </div>
    </StoreContext.Provider>
  )
}
