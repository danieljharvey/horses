export const doEvaluateExpression = (code: string) => ({
  type: 'DoEvaluateExpression' as const,
  code,
})

export const doBindExpression = (
  bindingName: string,
  code: string,
  updateProject: boolean
) => ({
  type: 'DoBindExpression' as const,
  bindingName,
  code,
  updateProject,
})

export type EditorEvent =
  | ReturnType<typeof doEvaluateExpression>
  | ReturnType<typeof doBindExpression>
