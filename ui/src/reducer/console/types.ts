export type Log = {
  message: string
  timestamp: number
}

export type ConsoleState = {
  logs: Log[]
}

export type ConsoleEvent = never
