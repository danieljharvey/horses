export const log = (
  message: string,
  timestamp: number
) => ({
  type: 'Log',
  message,
  timestamp,
})

export type ConsoleAction = ReturnType<typeof log>
