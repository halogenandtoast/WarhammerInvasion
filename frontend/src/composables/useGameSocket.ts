// Game-socket lifecycle: connect on mount, disconnect on unmount,
// reconnect when the route's gameId changes or auth identity flips.
//
// The store-side `game.connect()` is idempotent for the same identity
// — repeated calls during an auth bootstrap settle cleanly.

import { onBeforeUnmount, onMounted, watch, type ToRefs } from 'vue'
import { auth } from '../stores/auth'
import { game } from '../stores/game'

interface RouteProps {
  gameId: string
  inviteToken: string | null
  password: string | null
}

export function useGameSocket(props: RouteProps | ToRefs<RouteProps>) {
  // Accept either the raw props object (Vue unwraps refs in templates,
  // but inside a composable we want the live values) or toRefs of it.
  // The callers pass the `props` object directly from <script setup>,
  // which already gives reactive access via getters.
  const read = (): RouteProps => ({
    gameId: (props as RouteProps).gameId,
    inviteToken: (props as RouteProps).inviteToken,
    password: (props as RouteProps).password,
  })

  const connect = () => game.connect(read())

  onMounted(connect)
  onBeforeUnmount(() => game.disconnect())

  // Route-level navigation between two game views.
  watch(
    () => (props as RouteProps).gameId,
    () => connect(),
  )

  // Auth identity changes. The first firing covers the cold bootstrap
  // (ready flips false→true) so the initial connect in onMounted bails
  // harmlessly and this watcher opens the real socket once we know
  // whether we're authed or a guest.
  watch(
    () => [auth.ready.value, auth.accessToken.value] as const,
    () => {
      game.disconnect()
      connect()
    },
  )
}
