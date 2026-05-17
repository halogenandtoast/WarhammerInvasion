// Shared "known error code" sets for the lobby + game socket
// surfaces. The server emits a small finite vocabulary; anything else
// gets surfaced verbatim. Each call site supplies its own i18n
// translator so the same helper works under both `lobby.errors.*` and
// `game.errors.*`.

export const LOBBY_ERROR_CODES: ReadonlySet<string> = new Set([
  'name_too_short',
  'name_too_long',
  'invalid_password',
  'game_not_found',
  'game_is_private',
  'game_full',
  'wrong_password',
  'unauthorized',
  'maintenance_in_progress',
])

export const GAME_ERROR_CODES: ReadonlySet<string> = new Set([
  'not_seated',
  'deck_not_found',
  'deck_not_owned',
  'not_host',
  'not_ready',
  'already_started',
  'forbidden',
  'game_started',
  'game_not_started',
  'card_unknown',
  'zone_required',
  'target_required',
  'unauthorized',
])

export function mapKnown(
  code: string,
  known: ReadonlySet<string>,
  translate: (k: string) => string,
): string {
  return known.has(code) ? translate(code) : code
}
