<script setup lang="ts">
// Modal dialog asking for a private game's password. Used by the
// lobby when the user clicks "Join" on a private game card.

import { ref, watch } from 'vue'
import { useI18n } from 'vue-i18n'
import type { GameSummary } from '../api/protocol'

const props = defineProps<{ game: GameSummary | null }>()

const emit = defineEmits<{
  (e: 'submit', payload: { game: GameSummary; password: string }): void
  (e: 'cancel'): void
}>()

const { t } = useI18n({ useScope: 'global' })

const password = ref('')

// Reset the field every time a different game opens this prompt.
watch(() => props.game?.gameId, () => {
  password.value = ''
})

function submit() {
  const g = props.game
  if (!g) return
  const pw = password.value.trim()
  if (!pw) return
  emit('submit', { game: g, password: pw })
}
</script>

<template>
  <div v-if="game" class="pw-overlay" @click.self="emit('cancel')">
    <form class="pw-dialog" @submit.prevent="submit">
      <h3>{{ t('lobby.games.card.password_prompt', { name: game.name }) }}</h3>
      <input
        v-model="password"
        type="password"
        maxlength="60"
        autofocus
        :aria-label="t('lobby.games.new_form.password_label')"
      />
      <div class="pw-buttons">
        <button class="ghost" type="button" @click="emit('cancel')">
          {{ t('lobby.games.card.cancel') }}
        </button>
        <button class="primary" type="submit" :disabled="!password.trim()">
          {{ t('lobby.games.card.join_password') }}
        </button>
      </div>
    </form>
  </div>
</template>

<style scoped>
.pw-overlay {
  position: fixed;
  inset: 0;
  background: var(--overlay-medium);
  display: grid;
  place-items: center;
  z-index: var(--z-modal);
  padding: 1rem;
}

.pw-dialog {
  background: var(--bg-elev);
  border: 1px solid var(--border);
  border-radius: var(--radius-lg);
  padding: 1rem 1.1rem;
  display: flex;
  flex-direction: column;
  gap: 0.8rem;
  min-width: min(360px, 100%);
}

.pw-dialog h3 {
  margin: 0;
  font-size: 0.95rem;
}

.pw-dialog input {
  min-height: var(--tap-target);
  padding: 0.5rem 0.75rem;
  background: var(--bg);
  color: var(--fg);
  border: 1px solid var(--border);
  border-radius: var(--radius-md);
  font-size: 0.95rem;
}

.pw-dialog input:focus-visible {
  outline: 2px solid var(--accent-strong);
  border-color: var(--accent-strong);
}

.pw-buttons {
  display: flex;
  justify-content: flex-end;
  gap: 0.45rem;
}

.ghost {
  min-height: var(--tap-target);
  padding: 0 0.85rem;
  background: transparent;
  border: 1px solid var(--border);
  color: var(--fg-dim);
  border-radius: var(--radius-md);
  cursor: pointer;
  font-size: 0.85rem;
}

.ghost:hover {
  color: var(--fg);
  border-color: var(--fg-dim);
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
