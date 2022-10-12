export const updateCode = (text: string) => ({
  type: 'UpdateCode' as const,
  text,
})

export type EditorAction = ReturnType<typeof updateCode>
