// Thin fetch wrapper. Adds the bearer token, auto-refreshes on 401,
// and surfaces typed errors. Every JSON HTTP call in the SPA goes
// through here so the refresh dance + error-shape parsing live in one
// place.

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

export interface RequestOpts {
  method?: string
  body?: unknown
  signal?: AbortSignal
  // Skip the bearer header AND the 401 → refresh recursion. Used by
  // the auth endpoints themselves (register / login / refresh) so they
  // don't deadlock the refresh loop.
  unauthenticated?: boolean
}

async function rawFetch(path: string, opts: RequestOpts): Promise<Response> {
  const headers = new Headers()
  headers.set('Accept', 'application/json')
  if (opts.body !== undefined) headers.set('Content-Type', 'application/json')
  if (!opts.unauthenticated && auth.accessToken.value) {
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

function isObject(v: unknown): v is Record<string, unknown> {
  return v !== null && typeof v === 'object'
}

// Pull a server-reported error code out of the response body. Falls
// back to `http_<status>` when the body isn't JSON or doesn't carry an
// `error` field.
async function extractCode(res: Response): Promise<string> {
  try {
    const body = (await res.clone().json()) as unknown
    if (isObject(body) && typeof body.error === 'string') return body.error
  } catch {
    /* ignore */
  }
  return `http_${res.status}`
}

export async function api<T = unknown>(path: string, opts: RequestOpts = {}): Promise<T> {
  let res = await rawFetch(path, opts)
  if (res.status === 401 && !opts.unauthenticated) {
    const refreshed = await auth.refresh()
    if (refreshed) res = await rawFetch(path, opts)
  }
  if (res.status === 204) return undefined as T
  if (!res.ok) {
    throw new ApiError(res.status, await extractCode(res))
  }
  try {
    return (await res.json()) as T
  } catch {
    // 200 with empty body — treat as void.
    return undefined as T
  }
}

// Sugar for the common case: POST with a JSON body and skip auth (used
// by the auth endpoints themselves so they don't try to refresh on the
// way to refreshing).
export function postUnauthed<T = unknown>(path: string, body: unknown): Promise<T> {
  return api<T>(path, { method: 'POST', body, unauthenticated: true })
}
