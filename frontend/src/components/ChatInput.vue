<script setup lang="ts">
// Input box at the bottom of a chat panel. Two modes:
//
//   - authed:  text input + submit button. Emits `send` with the
//              trimmed message and clears the input.
//   - guest:   sign-in CTA. Emits `sign-in` when the button is hit.
//
// Used by both the lobby and the in-game messages rail. The styling
// matches both surfaces; the parent decides where the panel border /
// background lives.

import { ref } from 'vue'
import { useI18n } from 'vue-i18n'

const props = defineProps<{
  guest: boolean
  // Translation key prefix — expected keys are
  // `<prefix>.placeholder`, `<prefix>.send`, `<prefix>.guest_cta`.
  i18nPrefix: string
}>()

const emit = defineEmits<{
  (e: 'send', text: string): void
  (e: 'sign-in'): void
}>()

const { t } = useI18n({ useScope: 'global' })

const text = ref('')

function submit() {
  const trimmed = text.value.trim()
  if (!trimmed) return
  emit('send', trimmed)
  text.value = ''
}
</script>

<template>
  <form v-if="!props.guest" class="chat-input" @submit.prevent="submit">
    <input
      v-model="text"
      type="text"
      maxlength="1000"
      :placeholder="t(`${i18nPrefix}.placeholder`)"
      :aria-label="t(`${i18nPrefix}.placeholder`)"
    />
    <button class="primary" type="submit" :disabled="!text.trim()">
      {{ t(`${i18nPrefix}.send`) }}
    </button>
  </form>
  <div v-else class="chat-input chat-input-guest">
    <p class="guest-cta-text">{{ t(`${i18nPrefix}.guest_cta`) }}</p>
    <button class="primary" type="button" @click="emit('sign-in')">
      {{ t('app.nav.login') }}
    </button>
  </div>
</template>

<style scoped>
.chat-input {
  display: flex;
  gap: 0.5rem;
  padding: 0.55rem 0.7rem;
  border-top: 1px solid var(--border);
  background: var(--bg);
  flex-shrink: 0;
}

.chat-input input {
  flex: 1;
  min-height: var(--tap-target);
  padding: 0.4rem 0.7rem;
  background: var(--bg-elev);
  color: var(--fg);
  border: 1px solid var(--border);
  border-radius: var(--radius-md);
  font-size: 0.9rem;
}

.chat-input input:focus-visible {
  outline: 2px solid var(--accent-strong);
  border-color: var(--accent-strong);
}

.chat-input-guest {
  flex-direction: row;
  align-items: center;
  justify-content: space-between;
  gap: 0.55rem;
}

.guest-cta-text {
  margin: 0;
  color: var(--fg-dim);
  font-size: 0.82rem;
}

</style>
