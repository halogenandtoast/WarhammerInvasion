<script setup lang="ts">
// Single row in the lobby's "Open games" list. Encapsulates the
// badges, host meta, and the join CTA. The parent decides whether the
// CTA is enabled and what label / tooltip to show — this component is
// just the layout.

import { useI18n } from 'vue-i18n'
import type { GameSummary } from '../api/protocol'

defineProps<{
  game: GameSummary
  canJoin: boolean
  joinLabel: string
  blockedTooltip: string | null
  statusLabel: string
}>()

const emit = defineEmits<{ (e: 'join', g: GameSummary): void }>()

const { t } = useI18n({ useScope: 'global' })
</script>

<template>
  <li class="game-card">
    <div class="game-card-main">
      <div class="game-row">
        <h3>{{ game.name }}</h3>
        <div class="badges">
          <span
            class="badge"
            :class="game.visibility === 'Public' ? 'badge-public' : 'badge-private'"
          >
            {{
              game.visibility === 'Public'
                ? t('lobby.games.card.public_badge')
                : t('lobby.games.card.private_badge')
            }}
          </span>
          <span v-if="game.hasPassword" class="badge badge-pw">
            {{ t('lobby.games.card.password_badge') }}
          </span>
          <span v-if="game.allowSpectators" class="badge badge-spec">
            {{ t('lobby.games.card.spectators_badge') }}
          </span>
        </div>
      </div>
      <p class="game-meta">
        {{ t('lobby.games.card.host_by', { name: game.host.displayName }) }}
        · {{ t('lobby.games.card.seats_label', { filled: game.filledSeats }) }}
        · <span class="status-tag" :data-status="game.status">{{ statusLabel }}</span>
        <template v-if="game.spectatorCount > 0">
          · {{ t('lobby.games.card.spectators_count', { n: game.spectatorCount }) }}
        </template>
      </p>
    </div>
    <button
      class="primary"
      type="button"
      :disabled="!canJoin"
      :title="blockedTooltip ?? undefined"
      @click="emit('join', game)"
    >
      {{ joinLabel }}
    </button>
  </li>
</template>

<style scoped>
.game-card {
  display: flex;
  gap: 0.8rem;
  align-items: center;
  justify-content: space-between;
  padding: 0.7rem 0.85rem;
  background: var(--bg);
  border: 1px solid var(--border);
  border-radius: var(--radius-md);
}

.game-card-main {
  display: flex;
  flex-direction: column;
  gap: 0.2rem;
  min-width: 0;
}

.game-row {
  display: flex;
  align-items: center;
  gap: 0.5rem;
  flex-wrap: wrap;
}

.game-card h3 {
  margin: 0;
  font-size: 0.98rem;
  font-weight: 600;
  word-break: break-word;
}

.badges {
  display: inline-flex;
  gap: 0.25rem;
}

.badge {
  font-size: 0.7rem;
  padding: 0.1rem 0.55rem;
  border-radius: var(--radius-pill);
  border: 1px solid var(--border);
  color: var(--fg-dim);
  letter-spacing: 0.05em;
}

.badge-public {
  background: rgba(110, 155, 209, 0.18);
  border-color: rgba(110, 155, 209, 0.35);
  color: var(--faction-order);
}

.badge-private {
  background: rgba(196, 99, 74, 0.16);
  border-color: rgba(196, 99, 74, 0.36);
  color: var(--accent-strong);
}

.badge-pw {
  background: rgba(212, 179, 87, 0.12);
  border-color: rgba(212, 179, 87, 0.35);
  color: var(--race-empire);
}

.badge-spec {
  background: rgba(95, 160, 214, 0.10);
  border-color: rgba(95, 160, 214, 0.30);
  color: var(--race-high-elf, #5fa0d6);
}

.game-meta {
  margin: 0;
  font-size: 0.79rem;
  color: var(--fg-faint);
}

.status-tag[data-status='StatusPlaying'] {
  color: var(--race-orc);
}

.status-tag[data-status='StatusEnded'] {
  color: var(--fg-faint);
}

.primary {
  min-height: var(--tap-target);
  padding: 0 1rem;
  background: var(--accent);
  border: 1px solid var(--accent);
  color: var(--on-accent);
  border-radius: var(--radius-md);
  font-size: 0.9rem;
  cursor: pointer;
}

.primary:hover:not(:disabled) {
  background: var(--accent-strong);
  border-color: var(--accent-strong);
}

.primary:disabled {
  opacity: 0.55;
  cursor: not-allowed;
}
</style>
