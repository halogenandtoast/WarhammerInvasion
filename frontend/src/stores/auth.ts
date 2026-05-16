// Global auth state. Plain Vue refs in a module — Pinia would do the
// same job with more ceremony for a single store.
//
// Access token is kept in memory only (cleared on full reload). Refresh
// token rides as an HttpOnly cookie set by the backend; on cold start
// we POST /api/auth/refresh to mint a new access token if the cookie
// is still valid.

import { computed, ref } from 'vue'

export interface AuthUser {
  id: string
  email: string
  displayName: string
}

interface SessionResponse {
  accessToken: string
  expiresIn: number
  user: AuthUser
}

const _user = ref<AuthUser | null>(null)
const _accessToken = ref<string | null>(null)
const _ready = ref(false)

let inflightRefresh: Promise<boolean> | null = null

async function postJson<T>(path: string, body: unknown): Promise<T> {
  const res = await fetch(path, {
    method: 'POST',
    headers: { 'Content-Type': 'application/json', Accept: 'application/json' },
    body: JSON.stringify(body),
    credentials: 'include',
  })
  if (res.status === 401) throw new AuthError(res.status, await safeError(res))
  if (!res.ok) throw new AuthError(res.status, await safeError(res))
  if (res.status === 204) return undefined as T
  return (await res.json()) as T
}

async function safeError(res: Response): Promise<string> {
  try {
    const j = await res.json()
    if (j && typeof j === 'object' && typeof (j as { error?: string }).error === 'string') {
      return (j as { error: string }).error
    }
  } catch {
    /* ignore */
  }
  return `http_${res.status}`
}

export class AuthError extends Error {
  status: number
  code: string

  constructor(status: number, code: string) {
    super(code)
    this.status = status
    this.code = code
  }
}

function apply(session: SessionResponse) {
  _user.value = session.user
  _accessToken.value = session.accessToken
}

async function register(email: string, password: string, displayName: string) {
  const s = await postJson<SessionResponse>('/api/auth/register', {
    email,
    password,
    displayName,
  })
  apply(s)
}

async function login(email: string, password: string) {
  const s = await postJson<SessionResponse>('/api/auth/login', { email, password })
  apply(s)
}

async function logout() {
  try {
    await postJson<void>('/api/auth/logout', {})
  } catch {
    // server-side cleanup is best-effort; always drop local state
  }
  _user.value = null
  _accessToken.value = null
}

async function refresh(): Promise<boolean> {
  if (inflightRefresh) return inflightRefresh
  inflightRefresh = (async () => {
    try {
      const s = await postJson<SessionResponse>('/api/auth/refresh', {})
      apply(s)
      return true
    } catch {
      _user.value = null
      _accessToken.value = null
      return false
    } finally {
      inflightRefresh = null
    }
  })()
  return inflightRefresh
}

// Run once on app start. Tries to mint a fresh access token from the
// refresh cookie; failure is the normal "not signed in" path.
async function bootstrap() {
  if (_ready.value) return
  await refresh()
  _ready.value = true
}

export const auth = {
  user: computed(() => _user.value),
  accessToken: computed(() => _accessToken.value),
  isAuthenticated: computed(() => _user.value !== null),
  ready: computed(() => _ready.value),
  register,
  login,
  logout,
  refresh,
  bootstrap,
}
