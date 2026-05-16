<script setup lang="ts">
import { ref } from 'vue'
import { useI18n } from 'vue-i18n'
import { auth, AuthError } from '../stores/auth'

const { t } = useI18n({ useScope: 'global' })

const emit = defineEmits<{ (e: 'navigate', target: string): void }>()

const email = ref('')
const password = ref('')
const displayName = ref('')
const busy = ref(false)
const error = ref<string | null>(null)

async function submit() {
  if (busy.value) return
  if (password.value.length < 8) {
    error.value = t('auth.errors.password_too_short')
    return
  }
  busy.value = true
  error.value = null
  try {
    await auth.register(email.value.trim(), password.value, displayName.value.trim())
    emit('navigate', '#/decks')
  } catch (e) {
    error.value = e instanceof AuthError ? errorLabel(e.code) : t('auth.errors.network')
  } finally {
    busy.value = false
  }
}

function errorLabel(code: string): string {
  switch (code) {
    case 'email_in_use':
      return t('auth.errors.email_in_use')
    case 'invalid_email':
      return t('auth.errors.invalid_email')
    case 'password_too_short':
      return t('auth.errors.password_too_short')
    case 'password_too_long':
      return t('auth.errors.password_too_long')
    case 'invalid_display_name':
      return t('auth.errors.invalid_display_name')
    default:
      return code
  }
}
</script>

<template>
  <main class="auth-page">
    <form class="auth-card" @submit.prevent="submit" novalidate>
      <header>
        <p class="eyebrow">{{ t('auth.register.eyebrow') }}</p>
        <h1>{{ t('auth.register.heading') }}</h1>
        <p class="lead">{{ t('auth.register.lead') }}</p>
      </header>

      <label class="field">
        <span class="field-label">{{ t('auth.fields.display_name') }}</span>
        <input v-model="displayName" type="text" required maxlength="50" autocomplete="nickname" :placeholder="t('auth.fields.display_name_placeholder')" />
      </label>

      <label class="field">
        <span class="field-label">{{ t('auth.fields.email') }}</span>
        <input v-model="email" type="email" required autocomplete="email" />
      </label>

      <label class="field">
        <span class="field-label">{{ t('auth.fields.password') }}</span>
        <input v-model="password" type="password" required minlength="8" autocomplete="new-password" />
        <small class="hint">{{ t('auth.fields.password_hint') }}</small>
      </label>

      <p v-if="error" class="error">{{ error }}</p>

      <button class="primary" type="submit" :disabled="busy">
        {{ busy ? t('auth.register.submitting') : t('auth.register.submit') }}
      </button>

      <p class="switch">
        {{ t('auth.register.have_account') }}
        <a href="#/login" @click.prevent="emit('navigate', '#/login')">{{ t('auth.register.go_login') }}</a>
      </p>
    </form>
  </main>
</template>

<style scoped>
.auth-page {
  display: grid;
  place-items: start center;
  padding: 4rem 1rem;
  min-height: calc(100dvh - 60px);
  background: var(--bg);
}

.auth-card {
  width: 100%;
  max-width: 420px;
  display: flex;
  flex-direction: column;
  gap: 1rem;
  padding: 2rem;
  background: var(--bg-elev);
  border: 1px solid var(--border);
  border-radius: var(--radius-xl);
}

.eyebrow {
  margin: 0;
  font-size: 0.72rem;
  letter-spacing: 0.18em;
  text-transform: uppercase;
  color: var(--accent-strong);
}

h1 {
  margin: 0.4rem 0 0.3rem;
  font-size: 1.6rem;
  color: var(--fg);
}

.lead {
  margin: 0;
  color: var(--fg-dim);
}

.field {
  display: flex;
  flex-direction: column;
  gap: 0.3rem;
}

.field-label {
  font-size: 0.72rem;
  letter-spacing: 0.1em;
  text-transform: uppercase;
  color: var(--fg-faint);
}

.field input {
  min-height: var(--tap-target);
  padding: 0.55rem 0.85rem;
  background: var(--bg);
  color: var(--fg);
  border: 1px solid var(--border);
  border-radius: var(--radius-md);
  font-size: 1rem;
}

.field input:focus-visible {
  outline: 2px solid var(--accent-strong);
  outline-offset: 1px;
  border-color: var(--accent-strong);
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
  font-size: 1rem;
  cursor: pointer;
}

.primary:disabled {
  opacity: 0.6;
  cursor: progress;
}

.primary:hover:not(:disabled) {
  background: var(--accent-strong);
  border-color: var(--accent-strong);
}

.switch {
  margin: 0;
  color: var(--fg-dim);
  font-size: 0.9rem;
  text-align: center;
}

.switch a {
  color: var(--accent-strong);
  text-decoration: none;
}

.switch a:hover {
  text-decoration: underline;
}

.error {
  margin: 0;
  padding: 0.55rem 0.75rem;
  border-left: 3px solid var(--accent-strong);
  background: var(--bg);
  color: var(--fg);
  border-radius: var(--radius-sm);
  font-size: 0.9rem;
}
</style>
