// Hash-based router. The frontend is a single SPA with `#/`-style
// URLs; everything outside the auth flow can be reached by typing or
// pasting a URL, including deep links to a specific game with an
// invite token in the query string.
//
// The module exposes two things: a reactive `route` ref that always
// reflects the parsed location, and a `navigate(target)` helper that
// updates the location hash. `App.vue` mounts the matching view based
// on `route.value`.

import { ref } from 'vue'

export type Route =
  | { name: 'rules' }
  | { name: 'cards' }
  | { name: 'login' }
  | { name: 'register' }
  | { name: 'decks' }
  | { name: 'deck-view'; id: string }
  | { name: 'deck-edit'; id: string }
  | { name: 'lobby' }
  | { name: 'game'; id: string; inviteToken: string | null; password: string | null }

export type RouteName = Route['name']

const GAME_RE = /^games\/([\w-]+)(?:\?(.*))?$/
const DECK_EDIT_RE = /^decks\/([\w-]+)\/edit$/
const DECK_VIEW_RE = /^decks\/([\w-]+)$/

export function parseRoute(): Route {
  const hash = window.location.hash.replace(/^#\/?/, '')
  if (hash.startsWith('cards')) return { name: 'cards' }
  if (hash === 'login') return { name: 'login' }
  if (hash === 'register') return { name: 'register' }
  if (hash === 'decks') return { name: 'decks' }
  if (hash === 'rules') return { name: 'rules' }
  if (hash === 'lobby' || hash === '') return { name: 'lobby' }
  const gameMatch = GAME_RE.exec(hash)
  if (gameMatch) {
    const id = gameMatch[1]
    const qs = new URLSearchParams(gameMatch[2] ?? '')
    return {
      name: 'game',
      id,
      inviteToken: qs.get('t'),
      password: qs.get('password'),
    }
  }
  const edit = DECK_EDIT_RE.exec(hash)
  if (edit) return { name: 'deck-edit', id: edit[1] }
  const view = DECK_VIEW_RE.exec(hash)
  if (view) return { name: 'deck-view', id: view[1] }
  return { name: 'lobby' }
}

export const route = ref<Route>(parseRoute())

function refresh() {
  route.value = parseRoute()
}

export function navigate(target: string) {
  const hash = target.startsWith('#') ? target.slice(1) : target
  if (window.location.hash !== hash) {
    window.location.hash = hash
  }
  refresh()
}

// Auth-gated routes redirect to login when the user isn't signed in
// (and the auth bootstrap has settled). Returns the destination route
// the caller should actually render — usually `route.value`, but
// `'login'` for an unauthenticated visit to a protected page.
export type AuthCheck = { isAuthenticated: boolean; ready: boolean }

const AUTH_REQUIRED: ReadonlySet<RouteName> = new Set(['decks', 'deck-view', 'deck-edit'])

export function gateRoute(r: Route, check: AuthCheck): Route {
  if (AUTH_REQUIRED.has(r.name) && check.ready && !check.isAuthenticated) {
    return { name: 'login' }
  }
  return r
}

let installed = false
export function installRouter() {
  if (installed) return
  installed = true
  window.addEventListener('hashchange', refresh)
}

export function uninstallRouter() {
  if (!installed) return
  installed = false
  window.removeEventListener('hashchange', refresh)
}
