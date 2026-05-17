// Global auth state. Plain Vue refs in a module — Pinia would do the
// same job with more ceremony for a single store.
//
// Access token is kept in memory only (cleared on full reload). Refresh
// token rides as an HttpOnly cookie set by the backend; on cold start
// we POST /api/auth/refresh to mint a new access token if the cookie
// is still valid.

import { computed, ref } from 'vue'
import { ApiError, postUnauthed } from '../api/client'

export { ApiError as AuthError }

// Shape returned by the backend's `/api/auth/*` endpoints. See
// `userJsonInline` in `backend/src/Invasion/Server.hs`. NOTE: the field
// is `id`, not `userId` (the lobby/game wire types separately use
// `userId` for the same UUID, so don't confuse them).
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

function apply(session: SessionResponse) {
  _user.value = session.user
  _accessToken.value = session.accessToken
}

function clearSession() {
  _user.value = null
  _accessToken.value = null
}

async function register(email: string, password: string, displayName: string) {
  apply(await postUnauthed<SessionResponse>('/api/auth/register', { email, password, displayName }))
}

async function login(email: string, password: string) {
  apply(await postUnauthed<SessionResponse>('/api/auth/login', { email, password }))
}

async function logout() {
  try {
    await postUnauthed<void>('/api/auth/logout', {})
  } catch {
    // server-side cleanup is best-effort; always drop local state
  }
  clearSession()
}

async function refresh(): Promise<boolean> {
  if (inflightRefresh) return inflightRefresh
  inflightRefresh = (async () => {
    try {
      apply(await postUnauthed<SessionResponse>('/api/auth/refresh', {}))
      return true
    } catch {
      clearSession()
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
