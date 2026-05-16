# Claude guide — repo root

Online 1v1 implementation of the *Warhammer: Invasion* LCG.
See [ARCHITECTURE.md](./ARCHITECTURE.md) for the full design. Read it before
making non-trivial changes. This file is the short orientation.

## Layout

- `backend/` — Haskell (GHC2021, Yesod, Warp). Authoritative game engine
  and WebSocket server. See `backend/CLAUDE.md`.
- `frontend/` — Vue 3 + Vite + TypeScript SPA. Pure renderer; never owns
  game state. See `frontend/CLAUDE.md`.
- `ARCHITECTURE.md` — target design. Some pieces are not built yet; the doc
  flags them.
- `docs/rules/` — condensed Warhammer: Invasion LCG rules reference, written
  for agents implementing the engine. Start at
  [docs/rules/README.md](./docs/rules/README.md).

## Hard rules

1. **The server is authoritative.** All game state and rule enforcement
   live in `backend/src/Invasion/`. The frontend renders the last snapshot
   it received and forwards user intents — it never mutates `Game`.
2. **Games are ephemeral.** No DB. State lives in memory; idle games are
   swept after `WHI_IDLE_TTL_SECONDS` (default 300). Do not add persistence
   without a discussion.
3. **Debug must be opt-in.** `WHI_DEBUG=1` on the server and `VITE_DEBUG=1`
   (or `?debug=1`) on the client. Never default-on.
4. **Mobile-friendly is a constraint, not a stretch goal.** Tap targets
   ≥ 44 px, no hover-only affordances, test at 390 px wide.

## Common commands

```sh
# Backend
cd backend && stack build
cd backend && stack exec WarhammerInvasion-exe          # PORT=3000

# Frontend
cd frontend && npm install
cd frontend && npm run dev                              # http://localhost:5173
```

The Vite dev server proxies `/api/*` to the backend; the WebSocket route
(`/ws/games/:id`) will also need to be proxied once it lands.

## When making changes

- Touching the wire protocol (`backend/src/Invasion/Server/Protocol.hs`)
  means touching `frontend/src/api/protocol.ts` in the same change. The TS
  side is a mirror — keep them in lockstep.
- Engine changes that affect the JSON shape of `Game` will break the
  frontend's renderer; check the affected components in
  `frontend/src/components/`.
- New cards go in `backend/src/Invasion/CardDef.hs` and the lookup map in
  `backend/src/Invasion/Engine.hs` (`allCards`).

## Out of scope right now

Accounts, ranking, deck building UI, spectators, replays, persistence,
chat moderation. Push back if asked to add these casually.
