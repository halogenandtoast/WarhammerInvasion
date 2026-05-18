// Generic typed WebSocket client with auto-reconnect.
//
// Both the lobby and per-game sockets ride this. Auth is via a JWT in
// the query string (the only practical way to send credentials with a
// browser-initiated WS upgrade). Reconnect uses bounded exponential
// backoff capped at 30s.

export type SocketStatus = 'idle' | 'connecting' | 'open' | 'closed'

export interface SocketHandlers<TIn> {
  onMessage: (msg: TIn) => void
  onOpen?: () => void
  onClose?: (reason: 'going_away' | 'unauth' | 'error') => void
  onStatusChange?: (s: SocketStatus) => void
  // Called before each reconnect attempt. Lets the caller refresh
  // credentials (e.g. mint a new access token) before the URL is
  // rebuilt. Returning false aborts the reconnect — used to give up
  // when re-auth fails so we don't loop forever with a dead token.
  beforeReconnect?: () => Promise<boolean>
}

export interface TypedSocket<TOut> {
  send: (msg: TOut) => boolean
  close: () => void
  status: () => SocketStatus
}

interface OpenOpts {
  url: string
  // Resolved on every connection attempt so reconnects pick up a
  // freshly-refreshed token rather than reusing the original (which
  // may have expired since the socket first opened).
  getToken: () => string | null
}

const BACKOFFS_MS = [500, 1000, 2000, 5000, 10000, 30000]

export function openSocket<TIn, TOut>(
  opts: OpenOpts,
  handlers: SocketHandlers<TIn>,
): TypedSocket<TOut> {
  let ws: WebSocket | null = null
  let status: SocketStatus = 'idle'
  let closedByUser = false
  let attempt = 0
  let reconnectTimer: number | null = null

  const setStatus = (s: SocketStatus) => {
    if (s === status) return
    status = s
    handlers.onStatusChange?.(s)
  }

  const buildUrl = (): string => {
    const u = new URL(opts.url, window.location.href)
    // Force ws/wss scheme matching the page.
    u.protocol = window.location.protocol === 'https:' ? 'wss:' : 'ws:'
    const token = opts.getToken()
    if (token != null) u.searchParams.set('token', token)
    return u.toString()
  }

  const open = () => {
    if (closedByUser) return
    setStatus('connecting')
    let socket: WebSocket
    try {
      socket = new WebSocket(buildUrl())
    } catch (err) {
      console.warn('socket: failed to construct', err)
      scheduleReconnect()
      return
    }
    ws = socket

    socket.onopen = () => {
      attempt = 0
      setStatus('open')
      handlers.onOpen?.()
    }

    socket.onmessage = (e) => {
      const data = typeof e.data === 'string' ? e.data : ''
      if (!data) return
      let parsed: unknown = null
      try {
        parsed = JSON.parse(data)
      } catch {
        return
      }
      handlers.onMessage(parsed as TIn)
    }

    socket.onclose = (e) => {
      ws = null
      setStatus('closed')
      // Authentication failures don't recover by retrying.
      if (e.code === 1008 || e.reason === 'unauthorized') {
        handlers.onClose?.('unauth')
        return
      }
      if (closedByUser) {
        handlers.onClose?.('going_away')
        return
      }
      handlers.onClose?.('error')
      scheduleReconnect()
    }

    socket.onerror = () => {
      // Errors trigger onclose; nothing else to do here.
    }
  }

  const scheduleReconnect = () => {
    if (closedByUser) return
    const delay = BACKOFFS_MS[Math.min(attempt, BACKOFFS_MS.length - 1)]
    attempt += 1
    reconnectTimer = window.setTimeout(async () => {
      reconnectTimer = null
      if (closedByUser) return
      if (handlers.beforeReconnect) {
        let ok = false
        try {
          ok = await handlers.beforeReconnect()
        } catch {
          ok = false
        }
        if (closedByUser) return
        if (!ok) {
          handlers.onClose?.('unauth')
          return
        }
      }
      open()
    }, delay)
  }

  const close = () => {
    closedByUser = true
    if (reconnectTimer != null) {
      window.clearTimeout(reconnectTimer)
      reconnectTimer = null
    }
    if (ws) {
      try {
        ws.close(1000, 'client_close')
      } catch {
        /* ignore */
      }
      ws = null
    }
    setStatus('closed')
  }

  const send = (msg: TOut): boolean => {
    if (!ws || ws.readyState !== WebSocket.OPEN) return false
    ws.send(JSON.stringify(msg))
    return true
  }

  open()

  return { send, close, status: () => status }
}
