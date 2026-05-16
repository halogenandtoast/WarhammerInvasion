# Warhammer: Invasion — Architecture

An online, real-time 1v1 implementation of the *Warhammer: Invasion* LCG.

This document describes the **target** architecture. The repo currently has a
working game-engine skeleton (`backend/src/Invasion/`) and a Vite + Vue
scaffold (`frontend/`). Each section calls out what exists today vs. what is
yet to be built.

---

## 1. Goals and constraints

| # | Constraint | Implication |
|---|---|---|
| G1 | Real-time, two-player only | WebSockets; no matchmaking for >2; no turn-by-email fallback |
| G2 | Ephemeral games — no DB | All state lives in process memory; cleanup on idle |
| G3 | Authoritative server | Frontend is a thin renderer; never mutates game state locally |
| G4 | Mobile-friendly UI | Touch-first, responsive layout, no hover-only interactions |
| G5 | Debug mode for every feature | Engine exposes a typed "edit state" channel; UI exposes it behind a flag |
| G6 | Idle TTL: 5 min (configurable) | Both players disconnected → start countdown → drop the game |

Non-goals (for now): accounts, ranking/MMR, deck building UI, spectators,
chat moderation, replays, persistence across server restarts.

---

## 2. System overview

```
+----------------+        WebSocket (JSON)       +-----------------------+
|  Browser (P1)  | <---------------------------> |                       |
+----------------+                               |   Yesod / Warp        |
                                                 |   ─────────────       |
+----------------+        WebSocket (JSON)       |   Lobby + Sessions    |
|  Browser (P2)  | <---------------------------> |   Game runtime (STM)  |
+----------------+                               |   Engine (Queue+Msg)  |
                                                 +-----------------------+
                                                            |
                                                            v
                                                  in-memory game table
                                                  (TVar (Map GameId GameSlot))
```

- One process, one node. No external dependencies beyond the GHC runtime.
- Each active game = one `GameSlot` value in a shared `TVar` map, owned by a
  dedicated runner thread. All mutations go through that thread to keep the
  engine single-threaded per game.
- Browsers talk to the server only over WebSockets after the initial HTTP
  page-load + lobby creation.

---

## 3. Backend

Stack: GHC (GHC2021), Yesod-core, Warp, `wai-websockets`, STM, Aeson.
Already in `backend/package.yaml`: yesod-core, warp, aeson. Add:
`wai-websockets`, `websockets`, `stm`, `uuid`, `time`.

### 3.1 Module layout (target)

```
backend/src/
  Invasion/
    Engine.hs        -- pure-ish step function + Message ADT (exists)
    Game.hs          -- Game record + lenses (exists)
    Player.hs, Capital.hs, Card.hs, ...   (exist)
    Server/
      App.hs         -- Yesod foundation, route table
      WebSocket.hs   -- WS handler, frame codecs
      Session.hs     -- GameSlot, Client, runner thread
      Registry.hs    -- TVar (Map GameId GameSlot), TTL sweeper
      Protocol.hs    -- ClientMessage / ServerMessage ADTs + ToJSON/FromJSON
      Debug.hs       -- DebugCommand ADT, edit-state operations
    Server.hs        -- exposes runServer (exists; will be re-wired)
  Queue.hs           -- engine work-queue primitive (exists)
```

### 3.2 Game engine (existing)

The engine is a small message-driven interpreter:

- `Message` is a closed ADT of engine events (`Setup`, `Draw`, `BeginPhase`, …).
- `class Run a` lets each entity (`Player`, `Game`, eventually `Card`) react to
  messages by reading/writing local state and enqueuing more messages.
- `gameMain` pumps the queue until it's empty, then returns the resulting
  `Game`.

That model is preserved. The server runtime treats `gameMain` as the
"apply pending messages" step.

### 3.3 Game lifecycle and runner thread

```
create -> waiting -> playing -> finished -> abandoned -> swept
```

- **create**: `POST /api/games` (or a `CreateGame` WS frame on a lobby socket)
  builds two `Deck`s, calls `newGame`, allocates a `GameId` (UUID v4), inserts
  a `GameSlot` into the registry, spawns a runner thread, returns the join
  link.
- **waiting**: one seat filled, awaiting the second player.
- **playing**: both seats filled, runner processes player actions.
- **finished**: engine reports a winner; the slot stays in memory long enough
  for both clients to render the end screen, then is marked abandoned.
- **abandoned**: both player sockets closed. Idle timer starts.
- **swept**: TTL expired; slot dropped from registry.

```haskell
data GameSlot = GameSlot
  { gameId       :: GameId
  , env          :: TVar Env             -- engine state (queue + Game)
  , clients      :: TVar (Map PlayerKey Client)
  , inbox        :: TQueue Incoming      -- player actions + debug cmds
  , status       :: TVar SlotStatus
  , lastActivity :: TVar UTCTime
  }

data Client = Client
  { sendFrame :: ServerMessage -> IO ()
  , seat      :: PlayerKey
  , token     :: SeatToken                -- opaque, used to reconnect
  }
```

The runner loop is:

```haskell
forever $ do
  msg <- atomically (readTQueue inbox)
  applyIncoming msg            -- mutates env, may run gameMain
  broadcast =<< snapshotState  -- send updated Game to both seats
  updateLastActivity
```

Because the runner is the only writer, the engine itself does not need locks.

### 3.4 Connection model

| HTTP route | Purpose |
|---|---|
| `GET /api/health` | liveness (exists) |
| `POST /api/games` | create a new game, returns `{ gameId, seatTokens: [..] }` |
| `GET /api/games/:id` | summary (status, seats filled) — used by lobby page |
| `GET /ws/games/:id?token=<seatToken>` | WebSocket upgrade |

`seatToken` is opaque and bound to a `PlayerKey`. Reconnecting with the same
token re-attaches the client to its seat. Without a token a connection joins
as the next open seat (or is rejected if both are taken). Tokens are returned
once at creation and are not stored server-side beyond the slot's lifetime.

### 3.5 Protocol

JSON over WebSockets. One frame = one message. Versioned via top-level `v`.

**Client → Server** (`ClientMessage`):

```jsonc
{ "v": 1, "tag": "PlayerAction", "action": { "tag": "PassPhase" } }
{ "v": 1, "tag": "PlayerAction", "action": { "tag": "PlayCard", "card": "u1" } }
{ "v": 1, "tag": "Resync" }                                  // re-request state
{ "v": 1, "tag": "Debug", "cmd": { "tag": "SetResources", "player": "Player1", "n": 5 } }
```

**Server → Client** (`ServerMessage`):

```jsonc
{ "v": 1, "tag": "Welcome", "seat": "Player1", "gameId": "..." }
{ "v": 1, "tag": "State",   "game": { /* full Game JSON */ } }
{ "v": 1, "tag": "Prompt",  "id": "p1", "options": [ /* ... */ ] }
{ "v": 1, "tag": "Error",   "message": "Not your turn" }
{ "v": 1, "tag": "Closed",  "reason": "Both players disconnected" }
```

Design choices:

- **Full state, not deltas**, at least initially. The `Game` value is small,
  the engine already has a `ToJSON` instance, and full snapshots make the
  client trivial and reconnection free. Revisit if frames get large.
- **No optimistic UI.** The client renders the last `State` it received.
  Sending an action does *not* update local state — the server's reply does.
  This is what makes G3 (authoritative server) tractable.
- **Prompts are explicit.** When the engine pauses awaiting a player choice,
  it emits a `Prompt`. The client's only legal next message is a
  `PlayerAction` referencing that prompt. This keeps the UI from inventing
  actions out of band.

### 3.6 Idle cleanup

A single sweeper thread:

```haskell
forever $ do
  threadDelay (60 * 1_000_000)         -- 1 minute tick
  now <- getCurrentTime
  atomically $ do
    slots <- readTVar registry
    let dead = [ gid
               | (gid, s) <- Map.toList slots
               , noClientsAttached s
               , diffUTCTime now (lastActivity s) > ttl ]
    modifyTVar' registry (foldr Map.delete `flip` dead)
    -- signal runners to exit
```

`ttl` is read from env var `WHI_IDLE_TTL_SECONDS`, default 300. Sweeping when
*either* player is still attached is not allowed — `lastActivity` is only
relevant once both sockets are gone.

### 3.7 Debug mode

Debug is a first-class protocol channel, not a side door.

```haskell
data DebugCommand
  = SetResources    PlayerKey Int
  | SetBurdens      PlayerKey Int
  | SetHand         PlayerKey [CardCode]
  | MoveCard        UnitKey   Zone
  | ForcePhase      Phase
  | EnqueueMessage  Message              -- escape hatch
  | OverwriteGame   Game                 -- nuclear; gated by --allow-overwrite
```

Properties:

- `EnqueueMessage` lets a debugger push any `Message` straight into the
  engine queue. This is the recommended way to fix a misbehaving card —
  reproduce the bad state, then nudge it forward with the message that should
  have fired.
- `OverwriteGame` is intentionally crude. Pair it with an export-state
  command so a tester can copy → edit → paste back.
- Debug is enabled per-process via `WHI_DEBUG=1`. When disabled, the server
  rejects any `Debug` frame with an `Error`. Do **not** ship to production
  with it on by default.

### 3.8 What does *not* live on the backend

- Card art, layout, animation timings.
- Player display names and avatars (passed through, not validated).
- Anything cosmetic. The engine sends the *facts*; the client picks the look.

---

## 4. Frontend

Stack: Vue 3 (Composition API), Vite, TypeScript. Already scaffolded.
Add: Pinia (state), vue-router (lobby vs. game routes). Avoid heavy UI kits
— a card game's layout is bespoke.

### 4.1 Module layout (target)

```
frontend/src/
  main.ts
  App.vue
  router.ts
  api/
    http.ts            -- fetch helpers for /api/games
    socket.ts          -- typed WebSocket client (auto-reconnect)
    protocol.ts        -- TS mirror of ClientMessage / ServerMessage
  stores/
    game.ts            -- Pinia store; holds the latest server snapshot
    debug.ts           -- debug-panel state
  views/
    Lobby.vue          -- create / join
    Game.vue           -- board container
  components/
    Board.vue
    Capital.vue
    Hand.vue
    Card.vue
    PhaseTracker.vue
    Prompt.vue         -- renders server-issued Prompt frames
    DebugPanel.vue
  styles/
    tokens.css         -- spacing + colors
    layout.css         -- mobile-first grid
```

### 4.2 State management

One Pinia store, one source of truth: the last `State` frame from the server.
Everything is derived from it. No local board state, no optimistic moves.

```ts
type GameStore = {
  game: Game | null
  prompt: Prompt | null
  seat: PlayerKey | null
  connection: 'idle' | 'connecting' | 'open' | 'closed'
}
```

`api/socket.ts` owns the WebSocket and dispatches inbound frames to the
store. Outbound actions are sent via a single `send(action)` helper that
fails closed if the socket is not open.

### 4.3 Mobile-friendly layout

- CSS Grid with named areas; one column on narrow viewports, two columns
  (opponent / self) plus a side rail on wide ones.
- Tap targets ≥ 44 px. No hover-only affordances — every hover hint must
  also be reachable via long-press or an explicit tap-to-inspect.
- Use `@media (pointer: coarse)` to gate touch-specific layouts.
- Test against `viewport ≤ 390 px` (iPhone 12/13/14). If a card row doesn't
  fit, scroll it horizontally; do not shrink card text below 14 px.
- `100dvh` for full-screen, not `100vh`, to dodge mobile address-bar jank.

### 4.4 Debug UI

A `DebugPanel.vue` slide-in that:

- Is only mounted when `import.meta.env.VITE_DEBUG === '1'` or the URL
  contains `?debug=1`.
- Shows the raw `Game` JSON and lets you copy/paste it.
- Has buttons for each `DebugCommand` (set resources, set burdens, etc.).
- Has an `EnqueueMessage` text field that accepts JSON for a `Message`.

The panel sends `Debug` frames; everything else is read-only mirror of the
store. The panel is never the source of truth.

---

## 5. Reconnection and disconnect handling

| Event | Server behavior | Client behavior |
|---|---|---|
| Client socket closes | Mark seat detached; do **not** advance state | Auto-reconnect with backoff (1s, 2s, 4s, max 30s) |
| Client reconnects with valid token | Re-attach; send `Welcome` + current `State` | Resume; render last state |
| Both seats detached | Start idle timer (`lastActivity = now`) | Show "opponent disconnected" overlay |
| Both still detached after TTL | Sweep slot; future joins get 404 | Lobby shows game gone |
| Engine reports winner | Send terminal `State`; keep slot for ~60s | Show end screen |

The server never auto-resigns a player for disconnecting. Either they
reconnect or the slot is garbage-collected when both are gone.

---

## 6. Error handling and validation

- **Input validation** at the protocol boundary (`Protocol.hs`).
  Anything that parses as a `ClientMessage` is structurally valid; semantic
  validity (is it this player's turn? can they afford this card?) is the
  engine's job and surfaces as an `Error` frame.
- **Crashes in the runner thread** are caught and reported as a `Closed`
  frame with `reason = "Internal error"`. The slot is then swept.
- **Versioning**: every message carries `v: 1`. On mismatch the server
  rejects the upgrade with HTTP 400. We will introduce a `v: 2` rather than
  silently changing wire shapes.

---

## 7. Testing approach

- **Engine**: pure HUnit/Hspec tests over `Game` and `Message`. Set up a
  game, push messages, assert resulting state. The engine is the part most
  worth testing.
- **Protocol**: golden JSON tests for `ClientMessage` / `ServerMessage` so
  the TS mirror stays honest.
- **Session/runner**: a small integration test that opens two in-process WS
  clients, plays a scripted game, asserts both clients see the same final
  state.
- **Frontend**: component tests for `Card`, `Hand`, `Prompt`. The store is
  just a holder of server frames, so most logic is server-side.

---

## 8. Configuration

| Env var | Default | Notes |
|---|---|---|
| `PORT` | `3000` | HTTP listen port |
| `WHI_IDLE_TTL_SECONDS` | `300` | Idle game TTL (G6) |
| `WHI_DEBUG` | `0` | Enables `Debug` protocol frames |
| `WHI_ALLOW_OVERWRITE` | `0` | Enables `OverwriteGame` (requires `WHI_DEBUG=1`) |
| `BACKEND_PORT` | `3000` | Vite dev proxy target |
| `VITE_DEBUG` | unset | Mounts the debug panel in the SPA |

---

## 9. Out of scope (today)

- Persistence across restarts. Restarting the server drops all games.
- Authentication. Seat tokens are the only identity primitive.
- Spectators. Possible later by adding a `Spectator` seat type with
  read-only sockets.
- Deck building. Decks are hard-coded for now (`dwarfStarterDeck`).
- Cross-tab dedup. If you open the same seat in two tabs, both receive
  state; both can send actions. Last write wins.

---

## 10. Open questions

- Do we need delta updates before full-state snapshots become expensive?
  Punt until a single game JSON exceeds ~50 KB.
- Should `Prompt`s be queued or strictly one-at-a-time? Current design says
  one-at-a-time; this is simpler to reason about and matches how the LCG
  rules work in practice.
- Should we persist seat tokens to allow reconnects across server restarts?
  Probably yes once we have any kind of deployment story; not today.
