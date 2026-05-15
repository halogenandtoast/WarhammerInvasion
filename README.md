# WarhammerInvasion

Two-part project:

- `backend/` — Haskell/Yesod API. Game engine + a tiny HTTP surface.
- `frontend/` — Vue 3 + Vite + TypeScript SPA. Dev server proxies `/api/*` to the backend.

## Backend

```sh
cd backend
stack build
stack exec WarhammerInvasion-exe   # listens on $PORT (default 3000)
```

Routes:

- `GET  /api/health` — `{ "status": "ok" }`
- `POST /api/game`   — Builds two dwarf decks, runs setup, returns the resulting `Game` JSON.

## Frontend

```sh
cd frontend
npm install
npm run dev                        # http://localhost:5173
```

`vite.config.ts` proxies `/api` to `http://localhost:3000` (override with `BACKEND_PORT`).
