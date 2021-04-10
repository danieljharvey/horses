import * as React from 'react'
import { useEventReducer } from '../utils/useEventReducer'
import { eventReducer, initialState } from '../reducer/root'
import { runtime } from '../reducer/runtime'
import { View } from '../components/View'
import '../App.css'
import { useHistory } from 'react-router-dom'

interface Props {
    projectHash: string
}

// this doesn't feel right but YOLO
export const ProjectPage: React.FC<Props> = ({ projectHash }) => {
    const history = useHistory()

    const [state, dispatch] = useEventReducer(
        eventReducer,
        initialState(projectHash),
        runtime(history)
    )

    React.useEffect(() => {
        dispatch({ type: 'Initialise' })
        /* eslint-disable-next-line react-hooks/exhaustive-deps */
    }, [])

    return (
        <div className="App">
            <View state={state} dispatch={dispatch} />
        </div>
    )
}
