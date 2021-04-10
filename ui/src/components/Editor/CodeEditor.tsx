import * as React from 'react'
import MonacoEditor, {
  EditorDidMount,
  ChangeHandler,
} from 'react-monaco-editor'
import './CodeEditor.css'

import { mimsaLanguage } from './mimsaLanguageMonaco'

type Props = {
  code: string
  setCode: (s: string) => void
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

export const CodeEditor: React.FC<Props> = ({
  code,
  setCode,
}) => {
  const options = {
    selectOnLineNumbers: true,
    minimap: { enabled: false },
    automaticLayout: true,
    lineNumbers: 'off' as const,
    fontFamily: 'Iosevka',
    fontSize: 16,
    stickyTabStops: false,
    useTabStops: false,
    folding: false,
  }
  const editorDidMount: EditorDidMount = (
    editor,
    _monaco
  ) => {
    editor.focus()
  }

  const editorWillMount = (monaco: any) => {
    monaco.languages.register({ id: 'mimsa' })

    // Register a tokens provider for the language
    monaco.languages.setMonarchTokensProvider(
      'mimsa',
      mimsaLanguage
    )

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

  const onChange: ChangeHandler = (newValue, e) => {
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
