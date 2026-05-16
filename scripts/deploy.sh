#!/usr/bin/env bash
# deploy.sh — pull the latest revision on the droplet and rebuild the
# docker-compose stack.
#
# Resolves the droplet IP from `terraform output` (override with WHI_HOST=...).
# Pass --branch <ref> to deploy something other than what's already checked out.

set -euo pipefail

ROOT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd)"
TF_DIR="$ROOT_DIR/terraform"
SSH_USER="${WHI_SSH_USER:-root}"
REMOTE_DIR="${WHI_REMOTE_DIR:-/opt/whi}"
BRANCH=""

while [ $# -gt 0 ]; do
  case "$1" in
    --branch) BRANCH="${2:?--branch needs a ref}"; shift 2 ;;
    --branch=*) BRANCH="${1#--branch=}"; shift ;;
    -h|--help)
      sed -n '2,7p' "$0" | sed 's/^# \{0,1\}//'
      exit 0
      ;;
    *) echo "deploy.sh: unknown arg: $1" >&2; exit 2 ;;
  esac
done

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

echo "deploy.sh: target $SSH_USER@$HOST:$REMOTE_DIR${BRANCH:+ (branch $BRANCH)}"

ssh -o StrictHostKeyChecking=accept-new "$SSH_USER@$HOST" \
  REMOTE_DIR="$REMOTE_DIR" BRANCH="$BRANCH" bash -s <<'REMOTE'
set -euo pipefail
cd "$REMOTE_DIR"
if [ -n "$BRANCH" ]; then
  git fetch --depth 1 origin "$BRANCH"
  git checkout -B "$BRANCH" "origin/$BRANCH"
else
  git pull --ff-only
fi
echo "deploy.sh: HEAD now at $(git rev-parse --short HEAD) ($(git log -1 --pretty=%s))"
# Apply pending migrations (idempotent). Env is loaded from /etc/whi.env
# so DATABASE_URL points at the managed cluster.
set -a
. /etc/whi.env
set +a
dbmate --no-dump-schema up
docker compose up -d --build
docker image prune -f >/dev/null
docker compose ps
REMOTE

echo "deploy.sh: done — http://$HOST"
