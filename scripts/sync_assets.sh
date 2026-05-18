#!/usr/bin/env bash
# sync_assets.sh — push card images from frontend/public/ to the S3 bucket
# that backs the CloudFront distribution (see terraform/assets.tf).
#
# Only `cards/` is fetched from the CDN today (see frontend/src/lib/assets.ts
# — VITE_ASSETS_BASE_URL only prefixes /cards/* in the catalog views). The
# in-game CardArt component and the other public/ dirs (capitals,
# card-backs, tokens) are served from the SPA's own origin, so they don't
# need syncing. Pass --all to upload everything anyway, e.g. when prepping
# to move the SPA's own image fetches behind the CDN.
#
# Requires:
#   - `aws` CLI on PATH, already logged in (env vars / ~/.aws/credentials)
#   - terraform output for the bucket name, or WHI_ASSETS_BUCKET=<name>
#
# Env overrides:
#   WHI_ASSETS_BUCKET    bucket name (else read from `terraform output`)
#   WHI_ASSETS_DIST_ID   CloudFront distribution ID for --invalidate
#                        (else looked up from the terraform-output domain)
#
# Args:
#   --all          sync cards + capitals + card-backs + tokens
#   --dirs <list>  comma-separated subset under frontend/public/ (e.g.
#                  "cards,tokens"). Overrides the default ("cards").
#   --delete       pass --delete to `aws s3 sync` (removes S3 objects that
#                  are no longer present locally). Off by default — safer
#                  to leave orphan objects than to break a live game.
#   --dry-run      pass --dryrun to `aws s3 sync` (preview only)
#   --invalidate   issue a CloudFront invalidation for the synced paths
#                  after the upload completes
#
# Examples:
#   scripts/sync_assets.sh                       # sync /cards/ only
#   scripts/sync_assets.sh --all --invalidate    # full refresh + CDN bust
#   scripts/sync_assets.sh --dirs cards --dry-run

set -euo pipefail

ROOT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd)"
TF_DIR="$ROOT_DIR/terraform"
PUBLIC_DIR="$ROOT_DIR/frontend/public"

DIRS_DEFAULT="cards"
DIRS_ALL="cards,capitals,card-backs,tokens"
DIRS="$DIRS_DEFAULT"
EXTRA_SYNC_ARGS=()
INVALIDATE=0

while [ $# -gt 0 ]; do
  case "$1" in
    --all) DIRS="$DIRS_ALL"; shift ;;
    --dirs) DIRS="${2:?--dirs needs a comma-separated list}"; shift 2 ;;
    --dirs=*) DIRS="${1#--dirs=}"; shift ;;
    --delete) EXTRA_SYNC_ARGS+=(--delete); shift ;;
    --dry-run|--dryrun) EXTRA_SYNC_ARGS+=(--dryrun); shift ;;
    --invalidate) INVALIDATE=1; shift ;;
    -h|--help)
      sed -n '2,33p' "$0" | sed 's/^# \{0,1\}//'
      exit 0
      ;;
    *) echo "sync_assets.sh: unknown arg: $1" >&2; exit 2 ;;
  esac
done

if ! command -v aws >/dev/null 2>&1; then
  echo "sync_assets.sh: aws CLI not on PATH" >&2
  exit 1
fi

# --- Resolve bucket --------------------------------------------------------
BUCKET="${WHI_ASSETS_BUCKET:-}"
if [ -z "$BUCKET" ]; then
  if ! command -v terraform >/dev/null 2>&1; then
    echo "sync_assets.sh: terraform not on PATH and WHI_ASSETS_BUCKET not set" >&2
    exit 1
  fi
  BUCKET="$(cd "$TF_DIR" && terraform output -raw assets_bucket)"
fi
if [ -z "$BUCKET" ]; then
  echo "sync_assets.sh: could not resolve S3 bucket name" >&2
  exit 1
fi
echo "sync_assets.sh: bucket s3://$BUCKET"

# --- Sync each dir ---------------------------------------------------------
# Card filenames are content-addressable (e.g. core-066.jpg) so a long
# cache lifetime is safe. CloudFront's managed CachingOptimized policy
# already caps at 1y; this just tells browsers the same thing.
CACHE_CONTROL="public, max-age=31536000, immutable"

CHANGED_PREFIXES=()
IFS=',' read -r -a DIR_LIST <<<"$DIRS"
for dir in "${DIR_LIST[@]}"; do
  dir="${dir// /}"
  [ -z "$dir" ] && continue
  src="$PUBLIC_DIR/$dir"
  if [ ! -d "$src" ]; then
    echo "sync_assets.sh: skipping $dir — $src not found" >&2
    continue
  fi
  echo "sync_assets.sh: syncing $dir/ → s3://$BUCKET/$dir/"
  aws s3 sync "$src/" "s3://$BUCKET/$dir/" \
    --cache-control "$CACHE_CONTROL" \
    --no-progress \
    "${EXTRA_SYNC_ARGS[@]}"
  CHANGED_PREFIXES+=("/$dir/*")
done

# --- Optional CloudFront invalidation --------------------------------------
if [ "$INVALIDATE" -eq 1 ] && [ "${#CHANGED_PREFIXES[@]}" -gt 0 ]; then
  DIST_ID="${WHI_ASSETS_DIST_ID:-}"
  if [ -z "$DIST_ID" ]; then
    DOMAIN="$(cd "$TF_DIR" && terraform output -raw assets_cdn_domain)"
    DIST_ID="$(aws cloudfront list-distributions \
      --query "DistributionList.Items[?DomainName=='$DOMAIN'].Id | [0]" \
      --output text)"
  fi
  if [ -z "$DIST_ID" ] || [ "$DIST_ID" = "None" ]; then
    echo "sync_assets.sh: could not resolve CloudFront distribution ID; skipping invalidation" >&2
    exit 1
  fi
  echo "sync_assets.sh: invalidating ${CHANGED_PREFIXES[*]} on $DIST_ID"
  aws cloudfront create-invalidation \
    --distribution-id "$DIST_ID" \
    --paths "${CHANGED_PREFIXES[@]}" \
    --output text >/dev/null
fi

echo "sync_assets.sh: done"
