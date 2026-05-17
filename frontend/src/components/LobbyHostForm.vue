<script setup lang="ts">
// "Host a new game" form. Owns its own local state and emits a single
// `submit` payload — keeps Lobby.vue focused on coordinating the
// socket / errors / nav-after-create dance.

import { ref, watch } from 'vue'
import { useI18n } from 'vue-i18n'
import type { Visibility } from '../api/protocol'

const props = defineProps<{
  creating: boolean
}>()

const emit = defineEmits<{
  (e: 'submit', payload: {
    name: string
    visibility: Visibility
    password: string | null
    allowSpectators: boolean
    autoSkipActionWindows: boolean
  }): void
}>()

const { t } = useI18n({ useScope: 'global' })

const name = ref('')
const visibility = ref<Visibility>('Public')
const password = ref('')
// Spectator preference. Resets to the visibility-driven default each
// time visibility flips so the form matches the new default; the user
// can still override.
const allowSpectators = ref(true)
// Off by default — every action window waits for a manual pass unless
// the host opts in.
const autoSkip = ref(false)

watch(visibility, (v) => {
  allowSpectators.value = v === 'Public'
})

function onSubmit() {
  const trimmed = name.value.trim()
  if (!trimmed) return
  const pw = password.value.trim()
  emit('submit', {
    name: trimmed,
    visibility: visibility.value,
    password: visibility.value === 'Private' && pw.length > 0 ? pw : null,
    allowSpectators: allowSpectators.value,
    autoSkipActionWindows: autoSkip.value,
  })
}
</script>

<template>
  <form class="new-form" @submit.prevent="onSubmit">
    <label class="field">
      <span class="field-label">{{ t('lobby.games.new_form.name_label') }}</span>
      <input
        v-model="name"
        type="text"
        required
        maxlength="80"
        :placeholder="t('lobby.games.new_form.name_placeholder')"
      />
    </label>

    <fieldset class="vis-fieldset">
      <legend>{{ t('lobby.games.new_form.visibility_label') }}</legend>
      <label class="vis-option">
        <input v-model="visibility" type="radio" value="Public" />
        <span>
          <strong>{{ t('lobby.games.new_form.public') }}</strong>
          <small>{{ t('lobby.games.new_form.public_help') }}</small>
        </span>
      </label>
      <label class="vis-option">
        <input v-model="visibility" type="radio" value="Private" />
        <span>
          <strong>{{ t('lobby.games.new_form.private') }}</strong>
          <small>{{ t('lobby.games.new_form.private_help') }}</small>
        </span>
      </label>
    </fieldset>

    <label v-if="visibility === 'Private'" class="field">
      <span class="field-label">{{ t('lobby.games.new_form.password_label') }}</span>
      <input
        v-model="password"
        type="text"
        maxlength="60"
        :placeholder="t('lobby.games.new_form.password_placeholder')"
      />
      <small class="hint">{{ t('lobby.games.new_form.password_help') }}</small>
    </label>

    <label class="spec-option">
      <input v-model="allowSpectators" type="checkbox" />
      <span>
        <strong>{{ t('lobby.games.new_form.spectators_label') }}</strong>
        <small>{{ t('lobby.games.new_form.spectators_help') }}</small>
      </span>
    </label>

    <label class="spec-option">
      <input v-model="autoSkip" type="checkbox" />
      <span>
        <strong>{{ t('lobby.games.new_form.auto_skip_label') }}</strong>
        <small>{{ t('lobby.games.new_form.auto_skip_help') }}</small>
      </span>
    </label>

    <button class="primary" type="submit" :disabled="props.creating || !name.trim()">
      {{ props.creating ? t('lobby.games.new_form.submitting') : t('lobby.games.new_form.submit') }}
    </button>
  </form>
</template>

<style scoped>
.new-form {
  padding: 0.9rem 1rem;
  display: flex;
  flex-direction: column;
  gap: 0.7rem;
  border-bottom: 1px solid var(--border);
}

.field {
  display: flex;
  flex-direction: column;
  gap: 0.25rem;
}

.field-label {
  font-size: 0.7rem;
  letter-spacing: 0.1em;
  text-transform: uppercase;
  color: var(--fg-faint);
}

.field input {
  min-height: var(--tap-target);
  padding: 0.5rem 0.75rem;
  background: var(--bg);
  color: var(--fg);
  border: 1px solid var(--border);
  border-radius: var(--radius-md);
  font-size: 0.95rem;
}

.field input:focus-visible {
  outline: 2px solid var(--accent-strong);
  border-color: var(--accent-strong);
}

.vis-fieldset {
  border: 1px solid var(--border);
  border-radius: var(--radius-md);
  padding: 0.55rem 0.7rem;
  display: flex;
  flex-direction: column;
  gap: 0.45rem;
  background: var(--bg);
}

.vis-fieldset legend {
  font-size: 0.7rem;
  letter-spacing: 0.1em;
  text-transform: uppercase;
  color: var(--fg-faint);
  padding: 0 0.35rem;
}

.vis-option,
.spec-option {
  display: flex;
  gap: 0.55rem;
  align-items: flex-start;
  cursor: pointer;
}

.spec-option {
  padding: 0.45rem 0.7rem;
  border: 1px solid var(--border);
  border-radius: var(--radius-md);
  background: var(--bg);
}

.vis-option input[type='radio'],
.spec-option input[type='checkbox'] {
  margin-top: 0.32rem;
}

.vis-option strong,
.spec-option strong {
  font-size: 0.9rem;
  color: var(--fg);
}

.vis-option small,
.spec-option small {
  display: block;
  font-size: 0.78rem;
  color: var(--fg-faint);
  margin-top: 0.05rem;
}

.hint {
  font-size: 0.78rem;
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
