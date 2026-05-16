# Claude guide — frontend

Vue 3 + Vite + TypeScript SPA. Renders game state pushed from the
backend over WebSockets. **The frontend never owns game state.**

## Build / run

```sh
npm install
npm run dev          # http://localhost:5173, proxies /api to backend
npm run build        # vue-tsc + vite build
npm run preview
```

Vite proxy is configured in `vite.config.ts` (override backend target with
`BACKEND_PORT`). When the WebSocket route lands, the same proxy needs a
`ws: true` entry for `/ws`.

## Layout (target)

```
src/
  main.ts, App.vue, router.ts
  api/
    http.ts            -- typed fetch helpers
    socket.ts          -- WebSocket client w/ reconnect + backoff
    protocol.ts        -- TS mirror of backend's ClientMessage/ServerMessage
  stores/
    game.ts            -- Pinia: latest Game snapshot, current Prompt, seat
    debug.ts           -- debug-panel UI state
  views/
    Lobby.vue, Game.vue
  components/
    Board.vue, Capital.vue, Hand.vue, Card.vue,
    PhaseTracker.vue, Prompt.vue, DebugPanel.vue
  styles/
    tokens.css, layout.css
```

Today the repo only has the Vite + Vue starter scaffold (`App.vue`,
`HelloWorld.vue`). The above is what we're building toward — see
`../ARCHITECTURE.md` §4.

## State rule

One Pinia store, one source of truth: the last `State` frame from the
server. The store is **assigned to**, not computed. No optimistic UI.

```ts
// Bad — invents state locally
playCard(c) { this.game.hand = this.game.hand.filter(x => x !== c) }

// Good — ask the server, render whatever it sends back
playCard(c) { socket.send({ tag: 'PlayerAction', action: { tag: 'PlayCard', card: c } }) }
```

If a server reply doesn't arrive, the card stays in hand. That's correct
behavior — it tells the player something went wrong.

## Protocol mirror

`src/api/protocol.ts` mirrors the Haskell `ClientMessage` /
`ServerMessage` ADTs. When the backend's `Protocol.hs` changes, this file
changes in the same PR. Tag fields match constructor names verbatim
(Aeson's default for `deriveToJSON` is `{"tag": "Setup", ...}`).

## Mobile-friendly constraints

- Tap targets ≥ 44 px. No hover-only affordances — every hover hint must
  also be reachable via tap (long-press to inspect, etc.).
- CSS Grid with named areas. One column on narrow viewports, two
  columns (opponent/self) plus a side rail on wide.
- `100dvh`, not `100vh`, for full-screen layouts (address-bar jank).
- Test against 390 px wide. Horizontally scroll long card rows; never
  shrink card text below 14 px.
- Gate touch-specific behaviors with `@media (pointer: coarse)`.

## Debug panel

`DebugPanel.vue` is mounted only when:

- `import.meta.env.VITE_DEBUG === '1'`, or
- the URL contains `?debug=1`.

The panel emits `Debug` frames; it never mutates the game store directly.
It also displays the raw `Game` JSON for copy/paste workflows.

## Adding a component

A typical component:

```vue
<script setup lang="ts">
import { useGameStore } from '@/stores/game'
const game = useGameStore()
const props = defineProps<{ seat: PlayerKey }>()
</script>

<template>
  <section class="capital" v-if="game.game">
    ...
  </section>
</template>
```

Keep markup driven off the store. If you find yourself adding `ref()`
state that *describes the game*, stop — that belongs on the server.
Local UI state (open/closed panels, hovered card) is fine.
