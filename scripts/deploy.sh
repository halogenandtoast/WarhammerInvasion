#!/usr/bin/env bash
# deploy.sh — build images locally for linux/amd64, push to Docker Hub,
# then have the droplet pull + restart the docker-compose stack.
#
# Requires:
#   - `docker buildx` (default in modern Docker Desktop)
#   - `docker login` already done against Docker Hub
#   - terraform output for the droplet IP, or WHI_HOST=<ip>
#
# Env overrides:
#   WHI_DOCKERHUB_USER   Docker Hub namespace (default: halogenandtoast)
#   WHI_BUILD_PLATFORM   buildx platform (default: linux/amd64)
#   WHI_HOST             droplet IP (else read from `terraform output`)
#   WHI_SSH_USER         ssh user on the droplet (default: root)
#   WHI_REMOTE_DIR       path on the droplet (default: /opt/whi)
#
# Args:
#   --branch <ref>       check out a different git ref on the droplet
#                        (defaults to fast-forwarding the current branch)

set -euo pipefail

ROOT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd)"
TF_DIR="$ROOT_DIR/terraform"
SSH_USER="${WHI_SSH_USER:-root}"
REMOTE_DIR="${WHI_REMOTE_DIR:-/opt/whi}"
DOCKER_USER="${WHI_DOCKERHUB_USER:-halogenandtoast}"
PLATFORM="${WHI_BUILD_PLATFORM:-linux/amd64}"
BRANCH=""

while [ $# -gt 0 ]; do
  case "$1" in
    --branch) BRANCH="${2:?--branch needs a ref}"; shift 2 ;;
    --branch=*) BRANCH="${1#--branch=}"; shift ;;
    -h|--help)
      sed -n '2,21p' "$0" | sed 's/^# \{0,1\}//'
      exit 0
      ;;
    *) echo "deploy.sh: unknown arg: $1" >&2; exit 2 ;;
  esac
done

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

# --- Build & push both images via buildx -----------------------------------
BACKEND_IMAGE="$DOCKER_USER/whi-backend"
FRONTEND_IMAGE="$DOCKER_USER/whi-frontend"

# Make sure a multi-platform-capable builder exists. The default
# `docker-container` driver cross-builds via QEMU.
if ! docker buildx inspect whi-builder >/dev/null 2>&1; then
  docker buildx create --name whi-builder --driver docker-container --bootstrap >/dev/null
fi
docker buildx use whi-builder >/dev/null

echo "deploy.sh: building $BACKEND_IMAGE:{$SHA,latest} for $PLATFORM"
docker buildx build \
  --platform "$PLATFORM" \
  --tag "$BACKEND_IMAGE:$SHA" \
  --tag "$BACKEND_IMAGE:latest" \
  --push \
  "$ROOT_DIR/backend"

echo "deploy.sh: building $FRONTEND_IMAGE:{$SHA,latest} for $PLATFORM"
docker buildx build \
  --platform "$PLATFORM" \
  --tag "$FRONTEND_IMAGE:$SHA" \
  --tag "$FRONTEND_IMAGE:latest" \
  --push \
  "$ROOT_DIR/frontend"

# --- Tell the droplet to pull + restart ------------------------------------
echo "deploy.sh: target $SSH_USER@$HOST:$REMOTE_DIR${BRANCH:+ (branch $BRANCH)}"
ssh -o StrictHostKeyChecking=accept-new "$SSH_USER@$HOST" \
  REMOTE_DIR="$REMOTE_DIR" BRANCH="$BRANCH" bash -s <<'REMOTE'
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
echo "deploy.sh: HEAD now at $(git rev-parse --short HEAD) ($(git log -1 --pretty=%s))"
set -a
. /etc/whi.env
set +a
dbmate --no-dump-schema up
docker compose pull
docker compose up -d
docker image prune -f >/dev/null
docker compose ps
REMOTE

echo "deploy.sh: done — http://$HOST"
