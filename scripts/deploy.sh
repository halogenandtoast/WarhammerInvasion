#!/usr/bin/env bash
# deploy.sh — build images locally for linux/amd64, push to Docker Hub,
# then have the droplet pull + restart the docker-compose stack.
#
# Smart mode: the script diffs the local HEAD against the droplet's
# currently-deployed SHA. If only files under `frontend/` changed, only
# the frontend image is rebuilt and only the frontend container is
# recreated — the backend stays up, in-flight games survive. Otherwise
# the script posts a maintenance window (broadcast over every lobby /
# game WebSocket), waits --warn-minutes, then performs the rolling
# backend restart.
#
# Requires:
#   - `docker buildx` (default in modern Docker Desktop)
#   - `docker login` already done against Docker Hub
#   - terraform output for the droplet IP, or WHI_HOST=<ip>
#   - WHI_ADMIN_TOKEN set in /etc/whi.env on the droplet, so the
#     maintenance banner can be posted to /api/admin/maintenance. If
#     unset, the banner is skipped with a warning (the restart still
#     happens).
#
# Env overrides:
#   WHI_DOCKERHUB_USER   Docker Hub namespace (default: halogenandtoast)
#   WHI_BUILD_PLATFORM   buildx platform (default: linux/amd64)
#   WHI_HOST             droplet IP (else read from `terraform output`)
#   WHI_SSH_USER         ssh user on the droplet (default: root)
#   WHI_REMOTE_DIR       path on the droplet (default: /opt/whi)
#
# Args:
#   --branch <ref>          check out a different git ref on the droplet
#                           (defaults to fast-forwarding the current branch)
#   --warn-minutes <N>      minutes to show the maintenance banner before
#                           the rolling backend restart. Default 30. Pass
#                           0 to skip the banner and restart immediately
#                           (for hotfixes). Ignored in frontend-only mode.

set -euo pipefail

ROOT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd)"
TF_DIR="$ROOT_DIR/terraform"
SSH_USER="${WHI_SSH_USER:-root}"
REMOTE_DIR="${WHI_REMOTE_DIR:-/opt/whi}"
DOCKER_USER="${WHI_DOCKERHUB_USER:-halogenandtoast}"
PLATFORM="${WHI_BUILD_PLATFORM:-linux/amd64}"
BRANCH=""
WARN_MINUTES=30

while [ $# -gt 0 ]; do
  case "$1" in
    --branch) BRANCH="${2:?--branch needs a ref}"; shift 2 ;;
    --branch=*) BRANCH="${1#--branch=}"; shift ;;
    --warn-minutes) WARN_MINUTES="${2:?--warn-minutes needs a number}"; shift 2 ;;
    --warn-minutes=*) WARN_MINUTES="${1#--warn-minutes=}"; shift ;;
    -h|--help)
      sed -n '2,35p' "$0" | sed 's/^# \{0,1\}//'
      exit 0
      ;;
    *) echo "deploy.sh: unknown arg: $1" >&2; exit 2 ;;
  esac
done

case "$WARN_MINUTES" in
  ''|*[!0-9]*) echo "deploy.sh: --warn-minutes must be a non-negative integer" >&2; exit 2 ;;
esac

# --- Resolve droplet IP ----------------------------------------------------
HOST="${WHI_HOST:-}"
if [ -z "$HOST" ]; then
  if ! command -v terraform >/dev/null 2>&1; then
    echo "deploy.sh: terraform not on PATH and WHI_HOST not set" >&2
    exit 1
  fi
  HOST="$(cd "$TF_DIR" && terraform output -raw ipv4_address)"
fi
if [ -z "$HOST" ]; then
  echo "deploy.sh: could not resolve droplet IP" >&2
  exit 1
fi

# --- Resolve git SHA we're about to ship -----------------------------------
SHA="$(git -C "$ROOT_DIR" rev-parse --short HEAD)"
FULL_SHA="$(git -C "$ROOT_DIR" rev-parse HEAD)"
echo "deploy.sh: shipping $FULL_SHA ($SHA)"

if ! git -C "$ROOT_DIR" diff --quiet || ! git -C "$ROOT_DIR" diff --cached --quiet; then
  echo "deploy.sh: warning — working tree has uncommitted changes; they are NOT in the built image" >&2
fi

# --- Decide frontend-only vs full deploy -----------------------------------
# Ask the droplet what SHA it has, then diff the paths locally. If the
# droplet SHA isn't in our clone we fall back to a full deploy (safe).
echo "deploy.sh: querying droplet for currently-deployed SHA"
DROPLET_SHA="$(ssh -o StrictHostKeyChecking=accept-new "$SSH_USER@$HOST" \
  "cd '$REMOTE_DIR' && git rev-parse HEAD 2>/dev/null || true" \
  | tr -d '[:space:]')"

MODE="full"
if [ -z "$DROPLET_SHA" ]; then
  echo "deploy.sh: could not read droplet SHA; defaulting to full deploy"
elif [ "$DROPLET_SHA" = "$FULL_SHA" ]; then
  echo "deploy.sh: droplet already at $FULL_SHA; nothing to do"
  exit 0
elif ! git -C "$ROOT_DIR" cat-file -e "${DROPLET_SHA}^{commit}" 2>/dev/null; then
  echo "deploy.sh: droplet SHA $DROPLET_SHA not in local clone; defaulting to full deploy"
else
  CHANGED="$(git -C "$ROOT_DIR" diff --name-only "$DROPLET_SHA" "$FULL_SHA")"
  if [ -z "$CHANGED" ]; then
    echo "deploy.sh: no path diff between droplet and HEAD; defaulting to full deploy"
  else
    NON_FE="$(printf '%s\n' "$CHANGED" | grep -v '^frontend/' || true)"
    if [ -z "$NON_FE" ]; then
      MODE="frontend-only"
    fi
  fi
fi
echo "deploy.sh: deploy mode: $MODE"

# --- Build & push images via buildx ----------------------------------------
BACKEND_IMAGE="$DOCKER_USER/whi-backend"
FRONTEND_IMAGE="$DOCKER_USER/whi-frontend"

# Make sure a multi-platform-capable builder exists. The default
# `docker-container` driver cross-builds via QEMU.
if ! docker buildx inspect whi-builder >/dev/null 2>&1; then
  docker buildx create --name whi-builder --driver docker-container --bootstrap >/dev/null
fi
docker buildx use whi-builder >/dev/null

build_and_push() {
  local image="$1" ctx="$2"
  echo "deploy.sh: building $image:{$SHA,latest} for $PLATFORM"
  docker buildx build \
    --platform "$PLATFORM" \
    --tag "$image:$SHA" \
    --tag "$image:latest" \
    --push \
    "$ctx"
}

if [ "$MODE" = "full" ]; then
  build_and_push "$BACKEND_IMAGE"  "$ROOT_DIR/backend"
  build_and_push "$FRONTEND_IMAGE" "$ROOT_DIR/frontend"
else
  echo "deploy.sh: frontend-only — skipping backend image build"
  build_and_push "$FRONTEND_IMAGE" "$ROOT_DIR/frontend"
fi

# --- Tell the droplet to pull + restart ------------------------------------
echo "deploy.sh: target $SSH_USER@$HOST:$REMOTE_DIR${BRANCH:+ (branch $BRANCH)}"
ssh -o StrictHostKeyChecking=accept-new "$SSH_USER@$HOST" \
  REMOTE_DIR="$REMOTE_DIR" \
  BRANCH="$BRANCH" \
  MODE="$MODE" \
  WARN_MINUTES="$WARN_MINUTES" \
  bash -s <<'REMOTE'
set -euo pipefail
cd "$REMOTE_DIR"
# We still git-pull on the droplet so dbmate sees the latest migrations and
# docker compose reads the latest compose file. The images themselves come
# from Docker Hub, not from this checkout.
if [ -n "$BRANCH" ]; then
  git fetch --depth 1 origin "$BRANCH"
  git checkout -B "$BRANCH" "origin/$BRANCH"
else
  git pull --ff-only
fi
echo "deploy.sh[remote]: HEAD now at $(git rev-parse --short HEAD) ($(git log -1 --pretty=%s))"
set -a
. /etc/whi.env
set +a

post_maintenance() {
  local until_iso="$1"
  if [ -z "${WHI_ADMIN_TOKEN:-}" ]; then
    echo "deploy.sh[remote]: warning — WHI_ADMIN_TOKEN unset; skipping maintenance banner" >&2
    return 0
  fi
  curl -fsS -X POST http://localhost:3000/api/admin/maintenance \
    -H "Authorization: Bearer $WHI_ADMIN_TOKEN" \
    -H "Content-Type: application/json" \
    -d "{\"until\":\"$until_iso\",\"message\":\"Scheduled restart for a deploy.\"}" \
    >/dev/null
}

clear_maintenance() {
  if [ -z "${WHI_ADMIN_TOKEN:-}" ]; then return 0; fi
  curl -fsS -X DELETE http://localhost:3000/api/admin/maintenance \
    -H "Authorization: Bearer $WHI_ADMIN_TOKEN" >/dev/null || true
}

if [ "$MODE" = "frontend-only" ]; then
  # Caddy serves the SPA + proxies /api and /ws. Recreating the frontend
  # container briefly drops the listener — WS clients reconnect against
  # the still-running backend and pick up their in-memory game state.
  echo "deploy.sh[remote]: frontend-only restart — backend stays up"
  docker compose pull frontend
  docker compose up -d frontend
  docker image prune -f >/dev/null
  docker compose ps frontend
  exit 0
fi

# Full deploy: warn players, sleep, then roll the stack.
if [ "${WARN_MINUTES:-0}" -gt 0 ]; then
  until_iso="$(date -u -d "+${WARN_MINUTES} minutes" +%Y-%m-%dT%H:%M:%SZ)"
  echo "deploy.sh[remote]: posting maintenance banner; restart at $until_iso (${WARN_MINUTES}m)"
  trap 'clear_maintenance' EXIT
  post_maintenance "$until_iso"
  sleep "$((WARN_MINUTES * 60))"
fi

dbmate --no-dump-schema up
docker compose pull
docker compose up -d
docker image prune -f >/dev/null
docker compose ps
# Backend restarted from scratch so the maintenance TVar is already
# Nothing; clear_maintenance below is a belt-and-suspenders no-op in the
# happy path, and the meaningful clear happens via the EXIT trap if the
# script aborts during the warning window.
clear_maintenance
trap - EXIT
REMOTE

echo "deploy.sh: done — http://$HOST"
