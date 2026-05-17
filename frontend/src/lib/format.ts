// Tiny shared formatting helpers. Avoids three near-identical
// `formatTime` definitions sprinkled across Game.vue, Lobby.vue, etc.

export function formatTime(at: string): string {
  try {
    return new Date(at).toLocaleTimeString([], { hour: '2-digit', minute: '2-digit' })
  } catch {
    return ''
  }
}

// Zero-pad a small integer to two digits. Used by the maintenance
// countdown.
export function pad2(n: number): string {
  return n < 10 ? `0${n}` : String(n)
}
