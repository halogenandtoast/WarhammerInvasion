# Claude guide — backend

Haskell game engine + WebSocket server for *Warhammer: Invasion*.

## Build / run

```sh
stack build
DATABASE_URL=postgres://whi:whi@localhost:5432/whi?sslmode=disable \
WHI_JWT_SECRET=dev-secret \
stack exec WarhammerInvasion-exe        # listens on $PORT (default 3000)
stack test                              # once tests exist
```

The server now requires a Postgres connection at boot — `DATABASE_URL`
and `WHI_JWT_SECRET` are both mandatory. For local dev, the
docker-compose stack (`docker compose up -d db`) provides Postgres.

`fourmolu.yaml` is the formatter config. Default extensions are set in
`package.yaml` — assume `OverloadedRecordDot`, `NoFieldSelectors`,
`DuplicateRecordFields`, `BlockArguments`, `LambdaCase`, etc. are on.
`NoImplicitPrelude` is on, with `Invasion.Prelude` as the project prelude.
**`Invasion.Model` opts into `FieldSelectors`** so persistent's generated
accessors (`userEmail`, `deckCards`, …) are top-level functions; access
entity fields with those, not with `.email` dot syntax.

## Module map

```
Invasion/
  Prelude.hs            -- project prelude; re-imports the common surface
  Types.hs              -- enums + phantom-typed Ref
  Capital.hs            -- Battlefield / QuestZone records
  Player.hs             -- Player record + PlayerState
  Game.hs               -- Game record, HasGame class
  Card.hs, CardDef.hs   -- card definitions; .hs-boot for cyclic deps
  Matcher.hs            -- predicates over board state
  Modifier.hs           -- temporary effects
  Entity.hs             -- shared entity helpers
  Engine.hs             -- Message ADT, Run class, gameMain loop
  Server.hs             -- Yesod foundation: health, game, auth, decks
  DB.hs                 -- ConnectionPool helpers wrapping persistent
  Model.hs              -- persistent entities (User, Deck, RefreshToken)
  Auth/
    Password.hs         -- bcrypt hashing wrapper
    Jwt.hs              -- HS256 JWT issue + verify
Queue.hs                -- two-IORef work queue used by the engine
```

## Rules reference

The condensed game rules live in [`../docs/rules/`](../docs/rules/). When
adding new `Message` constructors or card behaviors, cross-check against:

- [game-structure.md](../docs/rules/game-structure.md) — zones, win/loss
- [turn-sequence.md](../docs/rules/turn-sequence.md) — phases, action windows
- [combat.md](../docs/rules/combat.md) — the 5 combat steps + Counterstrike
- [effects.md](../docs/rules/effects.md) — Action/Forced/Constant/keywords
- [cards.md](../docs/rules/cards.md) — card types, costs, developments
- [glossary.md](../docs/rules/glossary.md) — quick term lookup

The **Golden Rule** applies: a card's text overrides the rulebook. Engine
defaults should be overrideable by per-card hooks (the existing `Run` /
`Message` model already supports this).

## The engine model

The engine is a message-driven interpreter:

- `Message` is a closed sum type of engine events (`Setup`, `Draw`,
  `BeginPhase`, …). Add a constructor when you need a new event.
- `class Run a` lets each entity react: `receive` runs in
  `StateT a GameT ()` so it can mutate that entity's local state and
  `send` (push) more `Message`s into the queue.
- `gameMain` pumps the queue until empty.

When adding behavior, prefer **enqueuing a new `Message`** over inlining
the effect. That keeps the engine inspectable and matches how the debug
protocol's `EnqueueMessage` escape hatch is meant to work.

## What's there vs. what's planned

Built today:
- Engine skeleton, queue, dwarf starter deck, `runSetup`, JSON for `Game`.
- HTTP routes:
  - `GET /api/health`
  - `POST /api/game` (returns a fresh post-setup game; currently open)
  - `POST /api/auth/{register,login,refresh,logout}`, `GET /api/auth/me`
  - `GET/POST /api/decks`, `GET/PUT/DELETE /api/decks/<uuid>` (auth-gated)
- Postgres-backed user accounts with bcrypt + HS256 JWT auth. Refresh
  tokens are hashed (SHA-256) in `refresh_tokens`, rotated on every use,
  and delivered to the browser as an HttpOnly cookie.

Planned (see `../ARCHITECTURE.md` §3):
- `Invasion/Server/{WebSocket,Session,Registry,Debug}.hs`.
- TVar-backed game registry + idle TTL sweeper.
- WebSocket upgrade at `/ws/games/:id?token=…` (will reuse the JWT from
  `Invasion.Auth.Jwt`).

Dependencies to add when wiring WebSockets: `wai-websockets`, `websockets`,
`stm`. The DB stack (`persistent`, `persistent-postgresql`, `esqueleto`,
`bcrypt`, etc.) is already in `package.yaml`.

## Conventions

- Engine-side records use `NoFieldSelectors` + `OverloadedRecordDot`.
  Access with `p.hand`, never `hand p`.
- **Persistent entities are the exception**: `Invasion.Model` enables
  `FieldSelectors` so the generated accessors (`userEmail u`, `deckCards
  d`, …) are top-level functions. Don't try to use `u.email` on an entity.
- New non-entity record types want a `deriveToJSON defaultOptions ''Foo`
  block at the bottom of the module so the frontend can render them.
- Phantom-typed `Ref Target` / `Ref Source` distinguishes which side of an
  effect a reference is on. Don't collapse them.
- Cyclic dependencies between `Card` and `Game` are handled via `.hs-boot`
  files. Preserve them — don't try to merge those modules.
- Concurrency: per the architecture, one runner thread per game owns the
  engine state. The engine itself stays single-threaded; do not add MVars
  inside `Invasion.*` modules.

## DB workflow

The schema lives in `../db/migrations/*.sql`. Persistent's `mkPersist`
generates Haskell types that mirror the schema — it does **not** generate
or apply migrations.

When changing a column:

1. `dbmate new <slug>` from the repo root, write the SQL up/down.
2. `dbmate up` against your local DB.
3. Update the matching entity in `Invasion.Model`. Field name, sql type,
   nullability, and key type all need to line up.
4. Add an exported `EntityField` constructor to the export list if you
   added a new column (otherwise esqueleto code can't reference it).

Never call `runMigration` / `runMigrationSilent`.

## Protocol changes

`ClientMessage` and `ServerMessage` in `Invasion/Server/Protocol.hs` (when
it exists) are the wire contract. Any change there is also a change to
`frontend/src/api/protocol.ts`. Bump `v` rather than silently reshaping a
frame.

## Debug mode

Debug commands ride the same WebSocket as player actions, gated by
`WHI_DEBUG=1`. The intended workflow when a card misbehaves:

1. Reproduce the bad state in a normal game.
2. Use the debug panel to inspect / mutate `Game` directly, or use
   `EnqueueMessage` to push the `Message` that should have fired.
3. Capture the failing state and add a regression test in `test/`.

`OverwriteGame` is the nuclear option and additionally requires
`WHI_ALLOW_OVERWRITE=1`.
