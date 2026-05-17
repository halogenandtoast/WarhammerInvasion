# Claude guide — repo root

Online 1v1 implementation of the *Warhammer: Invasion* LCG.
See [ARCHITECTURE.md](./ARCHITECTURE.md) for the full design. Read it before
making non-trivial changes. This file is the short orientation.

## Layout

- `backend/` — Haskell (GHC2021, Yesod, Warp). Authoritative game engine
  and WebSocket server. Also owns the user / deck CRUD API. See
  `backend/CLAUDE.md`.
- `frontend/` — Vue 3 + Vite + TypeScript SPA. Pure renderer for game
  state; also hosts the deckbuilder + auth UI. See `frontend/CLAUDE.md`.
- `db/migrations/` — plain SQL migrations applied by
  [dbmate](https://github.com/amacneil/dbmate). The schema is owned here,
  not by persistent's auto-migration.
- `ARCHITECTURE.md` — target design. Some pieces are not built yet; the doc
  flags them.
- `docs/rules/` — condensed Warhammer: Invasion LCG rules reference, written
  for agents implementing the engine. Start at
  [docs/rules/README.md](./docs/rules/README.md).

## Hard rules

1. **The server is authoritative for game state.** All in-game state and
   rule enforcement live in `backend/src/Invasion/`. The frontend renders
   the last snapshot it received and forwards user intents — it never
   mutates `Game`.
2. **Live game state is ephemeral; user data is durable.** Games live in
   memory and are swept after `WHI_IDLE_TTL_SECONDS` (default 300).
   **Users and decks** live in Postgres (the `users` and `decks` tables).
   Don't move game state to the DB without a discussion.
3. **Migrations are owned by `db/migrations/*.sql`.** Persistent's
   `mkPersist` only generates Haskell types — never call `runMigration`.
   New schema = new dbmate file; update `backend/src/Invasion/Model.hs` in
   the same change.
4. **Debug must be opt-in.** `WHI_DEBUG=1` on the server and `VITE_DEBUG=1`
   (or `?debug=1`) on the client. Never default-on.
5. **Mobile-friendly is a constraint, not a stretch goal.** Tap targets
   ≥ 44 px, no hover-only affordances, test at 390 px wide.

## Common commands

```sh
# First-time setup
cp .env.example .env                       # then set WHI_JWT_SECRET
docker compose up -d db                    # bring up local postgres
docker compose run --rm migrate up         # apply migrations

# Backend (running outside compose — for fast iteration)
cd backend && stack build
cd backend && DATABASE_URL=postgres://whi:whi@localhost:5432/whi?sslmode=disable \
              WHI_JWT_SECRET=dev-secret \
              stack exec WarhammerInvasion-exe          # PORT=3000

# All-in-compose (pulls the published Hub images, no local build):
docker compose up                          # or `scripts/deploy.sh` to ship

# Frontend (always outside compose for HMR)
cd frontend && npm install
cd frontend && npm run dev                              # http://localhost:5173

# DB
dbmate new <slug>                          # scaffold a migration
dbmate up                                  # apply
dbmate down                                # roll back the latest
```

The Vite dev server proxies `/api/*` (including `/api/auth/*`) to the
backend; the WebSocket route (`/ws/games/:id`) will also need to be
proxied once it lands.

## When making changes

- Touching the wire protocol (`backend/src/Invasion/Server/Protocol.hs`)
  means touching `frontend/src/api/protocol.ts` in the same change. The TS
  side is a mirror — keep them in lockstep.
- Engine changes that affect the JSON shape of `Game` will break the
  frontend's renderer; check the affected components in
  `frontend/src/components/`.
- New cards go in `backend/src/Invasion/CardDef.hs` and the lookup map in
  `backend/src/Invasion/Engine.hs` (`allCards`).
- New schema = `dbmate new <slug>` *and* updates to
  `backend/src/Invasion/Model.hs` so persistent's view of the schema
  stays in lockstep.
- New auth-gated routes go through `requireUser` in `Invasion/Server.hs`.

## Out of scope right now

Ranking, spectators, replays, chat moderation. Push back if asked to add
these casually. Accounts and the deckbuilder are now in scope.
