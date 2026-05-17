// Reactive `Date.now()` ticked once per second. The maintenance
// countdown banner needs a clock that re-renders the surrounding
// component each tick; this is the smallest abstraction over
// setInterval that does that without leaking timers.

import { onBeforeUnmount, onMounted, ref } from 'vue'

export function useNow(intervalMs = 1000) {
  const now = ref(Date.now())
  let timer: ReturnType<typeof setInterval> | null = null
  onMounted(() => {
    timer = setInterval(() => {
      now.value = Date.now()
    }, intervalMs)
  })
  onBeforeUnmount(() => {
    if (timer) clearInterval(timer)
  })
  return now
}
