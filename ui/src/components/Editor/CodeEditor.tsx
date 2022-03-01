import * as React from 'react'
import MonacoEditor, {
  Monaco,
  OnMount,
  OnChange,
  BeforeMount,
} from '@monaco-editor/react'
import './CodeEditor.css'
import {
  ErrorLocation,
  SourceItem,
  TypedHoleResponse,
} from '../../types'
import * as O from 'fp-ts/Option'
import { pipe } from 'fp-ts/function'
import { mimsaLanguage } from './mimsaLanguageMonaco'
import {
  editor,
  languages,
} from 'monaco-editor/esm/vs/editor/editor.api'
import {
  createMarkerForTypedHole,
  createMarkerForError,
  chooseSourceSpan,
} from './utils/sourceSpanHelpers'

type Props = {
  code: string
  setCode: (s: string) => void
  sourceItems: SourceItem[]
  errorLocations: ErrorLocation[]
  typedHoleResponses: TypedHoleResponse[]
}

const colours = {
  snow: '#fffafa',
  black: '#333333',
  pink: '#FFC0CB',
  lightPink: '#FFB6C1',
  blue: '#6495ed',
  darkPink: '#DB7093',
  darkBlue: '#26428b',
}

const editorWillMount: BeforeMount = (monaco: Monaco) => {
  monaco.languages.register({ id: 'mimsa' })

  // Register a tokens provider for the language
  monaco.languages.setMonarchTokensProvider(
    'mimsa',
    mimsaLanguage as any // for some reason this works but the types are totally different
  )

  // show autofill for typed hole suggestions
  monaco.languages.registerCodeActionProvider('mimsa', {
    provideCodeActions: (
      model,
      _range,
      _context,
      _token
    ) => {
      const suggestions = mutableTypedHoleResponses.flatMap(
        (ths) =>
          ths.thSuggestions.map((sug) => ({
            title: `Replace ?${ths.thName} with ${sug}`,
            diagnostics: [createMarkerForTypedHole(ths)],
            kind: 'quickfix',
            edit: {
              edits: [
                {
                  resource: model.uri,
                  edit: {
                    range: createMarkerForTypedHole(ths),
                    text: sug,
                  },
                },
              ],
            },
            isPreferred: true,
          }))
      )

      return {
        actions: suggestions,
        dispose: () => {},
      }
    },
  })

  monaco.languages.registerHoverProvider('mimsa', {
    provideHover: (_model, position) => {
      const newHover = pipe(
        chooseSourceSpan([...mutableSourceItems], position),
        O.fold<SourceItem, languages.Hover>(
          (): languages.Hover => ({
            contents: [],
          }),
          ({
            siSourceSpan: {
              ssRowStart,
              ssRowEnd,
              ssColStart,
              ssColEnd,
            },
            siLabel,
          }) => ({
            range: new monaco.Range(
              ssRowStart,
              ssColStart,
              ssRowEnd,
              ssColEnd
            ),
            contents: [{ value: siLabel }],
          })
        )
      )
      return Promise.resolve(newHover)
    },
  })

  // Define a new theme that contains only rules that match this language
  monaco.editor.defineTheme('mimsa', {
    base: 'vs',
    inherit: true,
    rules: [
      {
        token: 'type.identifier',
        foreground: colours.blue,
        fontStyle: 'bold',
      },
      {
        token: 'keyword',
        foreground: colours.darkPink,
      },
      {
        token: 'string',
        foreground: colours.darkBlue,
      },
      {
        token: 'number',
        foreground: colours.darkBlue,
      },
      {
        token: 'operator',
        foreground: colours.black,
      },
    ],
    colors: {
      'editor.foreground': colours.black,
      'editor.background': colours.snow,
      'editor.selectionBackground': colours.pink,
      'editorCursor.foreground': colours.pink,
      'editor.lineHighlightBackground': colours.snow,
      'editorWhitespace.foreground': colours.blue,
      'scrollbar.shadow': colours.snow,
      'scrollbarSlider.background': colours.snow,
      'scrollbarSlider.hoverBackground': colours.snow,
      'scrollbarSlider.activeBackground': colours.snow,
    },
  })
}

const options = {
  selectOnLineNumbers: false,
  minimap: { enabled: false },
  automaticLayout: true,
  lineNumbers: 'off' as const,
  fontFamily: 'Iosevka, Courier',
  fontSize: 16,
  stickyTabStops: false,
  useTabStops: false,
  folding: false,
}

let mutableSourceItems: SourceItem[] = []
let mutableTypedHoleResponses: TypedHoleResponse[] = []

export const CodeEditor: React.FC<Props> = ({
  code,
  setCode,
  sourceItems,
  errorLocations,
  typedHoleResponses,
}) => {
  const monacoRef =
    React.useRef<editor.IStandaloneCodeEditor | null>(null)

  React.useEffect(() => {
    mutableSourceItems = sourceItems
    mutableTypedHoleResponses = typedHoleResponses
  }, [sourceItems, typedHoleResponses])

  React.useEffect(() => {
    if (monacoRef.current && monacoRef.current) {
      const markers = [
        ...typedHoleResponses.map(createMarkerForTypedHole),
        ...errorLocations.map(createMarkerForError),
      ]
      const model = monacoRef.current.getModel()
      model &&
        editor.setModelMarkers(model, 'Mimsa', markers)
    }
  }, [errorLocations, typedHoleResponses])

  const editorDidMount: OnMount = (editor) => {
    editor.focus()

    // here is the editor instance
    // you can store it in `useRef` for further usage
    monacoRef.current = editor
  }

  const onChange: OnChange = (newValue) => {
    setCode(newValue || '')
  }

  return (
    <section className="editor">
      <MonacoEditor
        language="mimsa"
        theme="mimsa"
        value={code}
        options={options}
        onChange={onChange}
        onMount={editorDidMount}
        beforeMount={editorWillMount}
      />
    </section>
  )
}
