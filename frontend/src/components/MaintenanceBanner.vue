<script setup lang="ts">
import { computed } from 'vue'
import { useI18n } from 'vue-i18n'
import type { MaintenanceState } from '../api/protocol'
import { useNow } from '../composables/useNow'
import { pad2 } from '../lib/format'

const props = defineProps<{ state: MaintenanceState | null }>()
const { t } = useI18n({ useScope: 'global' })

const now = useNow(1000)

const untilMs = computed(() => {
  if (!props.state) return null
  const ms = Date.parse(props.state.until)
  return Number.isNaN(ms) ? null : ms
})

const remaining = computed(() => {
  if (untilMs.value === null) return null
  return Math.max(0, untilMs.value - now.value)
})

// Coarse countdown — minutes and seconds is enough for a 30-minute
// deploy window; flips to "any moment now" once the deadline is past.
const countdown = computed(() => {
  if (remaining.value === null) return ''
  if (remaining.value <= 0) return t('maintenance.imminent')
  const totalSec = Math.floor(remaining.value / 1000)
  const min = Math.floor(totalSec / 60)
  const sec = totalSec % 60
  if (min >= 1) return t('maintenance.countdown_minutes', { min, sec: pad2(sec) })
  return t('maintenance.countdown_seconds', { sec })
})
</script>

<template>
  <aside v-if="state" class="maint-banner" role="status" aria-live="polite">
    <span class="maint-dot" aria-hidden="true" />
    <div class="maint-text">
      <strong>{{ t('maintenance.heading') }}</strong>
      <span class="maint-msg">
        {{ state.message ?? t('maintenance.default_message') }}
      </span>
      <span class="maint-time">{{ countdown }}</span>
    </div>
  </aside>
</template>

<style scoped>
.maint-banner {
  display: flex;
  align-items: flex-start;
  gap: 0.7rem;
  padding: 0.7rem 1rem;
  margin: 0 0 1rem;
  background: rgba(212, 179, 87, 0.12);
  border: 1px solid rgba(212, 179, 87, 0.4);
  border-left-width: 3px;
  border-radius: var(--radius-md);
  color: var(--fg);
  font-size: 0.9rem;
}

.maint-dot {
  width: 10px;
  height: 10px;
  margin-top: 0.35rem;
  border-radius: 50%;
  background: #d4b357;
  box-shadow: 0 0 0 3px rgba(212, 179, 87, 0.22);
  flex-shrink: 0;
}

.maint-text {
  display: flex;
  flex-direction: column;
  gap: 0.15rem;
  line-height: 1.35;
}

.maint-text strong {
  font-size: 0.95rem;
}

.maint-msg {
  color: var(--fg-dim);
}

.maint-time {
  color: var(--fg-faint);
  font-variant-numeric: tabular-nums;
  font-size: 0.82rem;
}
</style>
