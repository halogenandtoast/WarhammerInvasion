<script setup lang="ts">
// Connection-status pill used by both the lobby and game views to
// surface the underlying WebSocket state (`idle`, `connecting`, `open`,
// `closed`). The colour dot communicates state at a glance; the label
// is i18n-resolved at the call site.

import type { SocketStatus } from '../api/socket'

defineProps<{
  status: SocketStatus
  label: string
}>()
</script>

<template>
  <span class="status-pill" :data-status="status">
    <span class="status-dot" aria-hidden="true" />
    {{ label }}
  </span>
</template>

<style scoped>
.status-pill {
  display: inline-flex;
  align-items: center;
  gap: 0.45rem;
  height: 28px;
  padding: 0 0.7rem;
  background: var(--bg-elev);
  border: 1px solid var(--border);
  border-radius: var(--radius-pill);
  color: var(--fg-dim);
  font-size: 0.78rem;
}

.status-dot {
  width: 8px;
  height: 8px;
  border-radius: 50%;
  background: var(--fg-faint);
}

.status-pill[data-status='open'] .status-dot {
  background: #5da46a;
  box-shadow: 0 0 0 3px rgba(93, 164, 106, 0.18);
}

.status-pill[data-status='connecting'] .status-dot {
  background: #d8b66c;
  animation: status-pulse 1.2s infinite;
}

.status-pill[data-status='closed'] .status-dot {
  background: var(--accent-strong);
}

@keyframes status-pulse {
  0%, 100% { opacity: 1; }
  50% { opacity: 0.35; }
}
</style>
