<script setup lang="ts">
// Top strip on the in-game page. Holds the back link, the game name +
// host, the WS status pill, spectator badges, and the high-level
// "what's happening" phrase shown to the right.

import { useI18n } from 'vue-i18n'
import type { GameView } from '../api/protocol'
import type { SocketStatus } from '../api/socket'
import StatusPill from './StatusPill.vue'

defineProps<{
  view: GameView | null
  socketStatus: SocketStatus
  spectatorCount: number
  isSpectator: boolean
  phaseHeading: string
}>()

const emit = defineEmits<{ (e: 'back'): void }>()

const { t } = useI18n({ useScope: 'global' })
</script>

<template>
  <header class="game-bar">
    <button class="back-link" type="button" @click="emit('back')">
      ← {{ t('game.back_to_lobby') }}
    </button>
    <div class="game-bar-title">
      <strong v-if="view">{{ view.name }}</strong>
      <span v-if="view" class="dim">·</span>
      <span v-if="view" class="dim">{{ t('game.hosted_by', { name: view.host.displayName }) }}</span>
    </div>
    <div class="game-bar-right">
      <StatusPill :status="socketStatus" :label="t(`lobby.status.${socketStatus}`)" />
      <span
        v-if="spectatorCount > 0"
        class="spec-pill"
        :title="t('game.spectators.tooltip', { n: spectatorCount })"
      >
        <span class="spec-eye" aria-hidden="true">👁</span>
        {{ t('game.spectators.count', { n: spectatorCount }) }}
      </span>
      <span v-if="isSpectator" class="spec-tag">{{ t('game.spectators.you_tag') }}</span>
      <span class="phase-heading">{{ phaseHeading }}</span>
    </div>
  </header>
</template>

<style scoped>
.game-bar {
  display: flex;
  align-items: center;
  gap: 0.75rem;
  padding: 0.45rem 0.9rem;
  background: var(--bg-elev);
  border-bottom: 1px solid var(--border);
  min-height: 44px;
  flex-wrap: nowrap;
}

.back-link {
  background: transparent;
  border: none;
  color: var(--fg-dim);
  cursor: pointer;
  padding: 0.2rem 0.3rem;
  font-size: 0.86rem;
}
.back-link:hover { color: var(--fg); }

.game-bar-title {
  display: flex;
  align-items: baseline;
  gap: 0.45rem;
  min-width: 0;
  flex: 1;
}
.game-bar-title strong {
  font-size: 0.98rem;
  font-weight: 600;
  white-space: nowrap;
  overflow: hidden;
  text-overflow: ellipsis;
}
.game-bar-title .dim { color: var(--fg-faint); font-size: 0.82rem; }

.game-bar-right {
  display: flex;
  align-items: center;
  gap: 0.6rem;
  flex-shrink: 0;
}

.spec-pill {
  display: inline-flex;
  align-items: center;
  gap: 0.3rem;
  height: 24px;
  padding: 0 0.55rem;
  background: rgba(95, 160, 214, 0.10);
  border: 1px solid rgba(95, 160, 214, 0.30);
  border-radius: var(--radius-pill);
  color: var(--race-high-elf, #5fa0d6);
  font-size: 0.74rem;
  font-variant-numeric: tabular-nums;
}
.spec-eye { font-size: 0.85rem; line-height: 1; }

.spec-tag {
  display: inline-flex;
  align-items: center;
  height: 24px;
  padding: 0 0.55rem;
  background: rgba(212, 179, 87, 0.16);
  border: 1px solid rgba(212, 179, 87, 0.4);
  border-radius: var(--radius-pill);
  color: var(--race-empire, #d4b357);
  font-size: 0.72rem;
  letter-spacing: 0.06em;
  text-transform: uppercase;
}

.phase-heading {
  font-size: 0.78rem;
  color: var(--fg-faint);
  letter-spacing: 0.06em;
  text-transform: uppercase;
}
</style>
