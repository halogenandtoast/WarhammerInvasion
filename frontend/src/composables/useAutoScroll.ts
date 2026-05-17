// Auto-scroll an element to its bottom whenever a watched source
// changes — used by chat lists and the in-game message log. Keeps the
// "scroll to bottom on new message" behaviour out of every consumer.

import { nextTick, ref, watch, type Ref, type WatchSource } from 'vue'

export function useAutoScroll<T>(source: WatchSource<T>) {
  const el = ref<HTMLElement | null>(null)
  watch(source, async () => {
    await nextTick()
    const node = el.value
    if (node) node.scrollTop = node.scrollHeight
  })
  return el as Ref<HTMLElement | null>
}
