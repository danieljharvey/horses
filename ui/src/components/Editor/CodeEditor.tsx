import * as React from 'react'
import MonacoEditor, {
  EditorDidMount,
  ChangeHandler,
  EditorWillMount,
} from 'react-monaco-editor'
import './CodeEditor.css'
import { SourceItem } from '../../types'
import * as Arr from 'fp-ts/Array'
import * as O from 'fp-ts/Option'
import { pipe } from 'fp-ts/function'
import { mimsaLanguage } from './mimsaLanguageMonaco'
import {
  Position,
  languages,
} from 'monaco-editor/esm/vs/editor/editor.api'

type Props = {
  code: string
  setCode: (s: string) => void
  sourceItems: SourceItem[]
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

const sourceSize = ({
  ssRowStart,
  ssRowEnd,
  ssColStart,
  ssColEnd,
}: SourceItem['siSourceSpan']): number => {
  const width = Math.max(ssRowEnd - ssRowStart, 1)
  const height = Math.max(ssColEnd - ssColStart, 1)
  return width * height
}

const chooseSourceSpan = (
  sourceItems: SourceItem[],
  position: Position
): O.Option<SourceItem> =>
  Arr.head(
    sourceItems
      .filter(
        ({
          siSourceSpan: {
            ssRowStart,
            ssRowEnd,
            ssColStart,
            ssColEnd,
          },
        }) =>
          position.lineNumber >= ssRowStart &&
          position.lineNumber <= ssRowEnd &&
          position.column >= ssColStart &&
          position.column <= ssColEnd
      )
      .sort(
        (a, b) =>
          sourceSize(a.siSourceSpan) -
          sourceSize(b.siSourceSpan)
      )
  )

const editorWillMount: EditorWillMount = (monaco) => {
  monaco.languages.register({ id: 'mimsa' })

  // Register a tokens provider for the language
  monaco.languages.setMonarchTokensProvider(
    'mimsa',
    mimsaLanguage as any // for some reason this works but the types are totally different
  )

  monaco.languages.registerHoverProvider('mimsa', {
    provideHover: (_model: unknown, position: Position) =>
      pipe(
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
      ),
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
  selectOnLineNumbers: true,
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

export const CodeEditor: React.FC<Props> = ({
  code,
  setCode,
  sourceItems,
}) => {
  React.useEffect(() => {
    mutableSourceItems = sourceItems
  }, [sourceItems])

  const editorDidMount: EditorDidMount = (editor) => {
    editor.focus()
  }

  const onChange: ChangeHandler = (newValue) => {
    setCode(newValue)
  }

  return (
    <section className="editor">
      <MonacoEditor
        language="mimsa"
        theme="mimsa"
        value={code}
        options={options}
        onChange={onChange}
        editorDidMount={editorDidMount}
        editorWillMount={editorWillMount}
      />
    </section>
  )
}
