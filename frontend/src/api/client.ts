// Thin fetch wrapper. Adds the bearer token, auto-refreshes on 401,
// and surfaces typed errors. Everything goes through here so the
// refresh dance lives in one place.

import { auth } from '../stores/auth'

export class ApiError extends Error {
  status: number
  code: string

  constructor(status: number, code: string, message?: string) {
    super(message ?? code)
    this.status = status
    this.code = code
  }
}

interface RequestOpts {
  method?: string
  body?: unknown
  signal?: AbortSignal
  // Skip auto-refresh recursion on the refresh call itself.
  noRefresh?: boolean
}

async function rawFetch(path: string, opts: RequestOpts): Promise<Response> {
  const headers = new Headers()
  headers.set('Accept', 'application/json')
  if (opts.body !== undefined) headers.set('Content-Type', 'application/json')
  if (auth.accessToken.value) {
    headers.set('Authorization', `Bearer ${auth.accessToken.value}`)
  }
  return fetch(path, {
    method: opts.method ?? 'GET',
    headers,
    body: opts.body === undefined ? null : JSON.stringify(opts.body),
    credentials: 'include',
    signal: opts.signal,
  })
}

export async function api<T = unknown>(path: string, opts: RequestOpts = {}): Promise<T> {
  let res = await rawFetch(path, opts)
  if (res.status === 401 && !opts.noRefresh) {
    const refreshed = await auth.refresh()
    if (refreshed) res = await rawFetch(path, opts)
  }
  if (res.status === 204) return undefined as T
  let bodyJson: unknown = null
  try {
    bodyJson = await res.json()
  } catch {
    // fall through; ApiError will have no body
  }
  if (!res.ok) {
    const code = (isObject(bodyJson) && typeof bodyJson.error === 'string'
      ? bodyJson.error
      : `http_${res.status}`)
    throw new ApiError(res.status, code)
  }
  return bodyJson as T
}

function isObject(v: unknown): v is Record<string, unknown> {
  return v !== null && typeof v === 'object'
}
